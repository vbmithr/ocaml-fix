open Rresult
open Core
open Async

open Bs_devkit
open Fix

let src = Logs.Src.create "fix.async"

let with_connection
  ?(tmpbuf=Bytes.create 4096) uri =
  let client_read, msg_write = Pipe.create () in
  let msg_read, client_write = Pipe.create () in
  let cleanup () =
      Logs_async.warn
        (fun m -> m "with_connection: cleanup up") >>| fun () ->
      Pipe.close_read msg_read ;
      Pipe.close msg_write in
  don't_wait_for (Pipe.closed client_write >>= cleanup) ;
  let run r w =
    let handle_chunk msgbuf ~pos ~len =
      if len > Bytes.length tmpbuf then
        failwith "Message bigger than tmpbuf" ;
      Bigstring.To_bytes.blito
        ~src:msgbuf ~src_pos:pos ~src_len:len ~dst:tmpbuf () ;
      let msg_str =
        Bytes.unsafe_to_string
          ~no_mutation_while_string_reachable:tmpbuf in
      begin match Fix.read msg_str ~len with
      | Error msg ->
        Logs_async.err ~src begin fun m ->
          m "<- Invalid message received (%a): %s" R.pp_msg msg msg_str
        end
      | Ok msg ->
        Logs_async.debug ~src begin fun m ->
          m "<- %a" pp msg
        end >>= fun () ->
        Pipe.write msg_write msg
      end >>| fun () ->
      `Consumed (len, `Need_unknown)
    in
    don't_wait_for @@
    Pipe.transfer msg_read Writer.(pipe w) ~f:begin fun msg ->
      Logs.debug ~src (fun m -> m "-> %a" pp msg) ;
      Fix.to_bytes msg
    end ;
    Reader.read_one_chunk_at_a_time r ~handle_chunk >>= fun _ ->
    (* TODO: cases *)
    Deferred.unit
  in
  don't_wait_for begin
    addr_of_uri uri >>= fun addr ->
    Monitor.try_with_or_error begin fun () ->
      Conduit_async.V2.with_connection addr run
    end >>= function
    | Error e ->
      Logs_async.err ~src (fun m -> m "%a" Error.pp e) >>=
      cleanup
    | Ok _ ->
      Logs_async.info ~src
        (fun m -> m "TCP connection terminated") >>=
      cleanup
  end;
  return (client_read, client_write)

module BoundedIntMap : sig
  type 'a t

  val empty : int -> 'a t
  val add : 'a t -> key:int -> data:'a -> 'a t
  (* val find : 'a t -> int -> 'a option *)
end = struct
  type 'a t = {
    m : 'a Int.Map.t ;
    length : int ;
  }

  let empty length = { m = Int.Map.empty ; length }

  let add ({ m ; length } as t) ~key ~data =
    match Int.Map.add m ~key ~data with
    | `Duplicate -> t
    | `Ok m ->
      if Int.Map.length m < length then
        { t with m }
      else
        let m = Int.Map.(remove m (fst (min_elt_exn m))) in
        { t with m }

  (* let find { m ; _ } k = Int.Map.find m k *)
end

let with_connection_ez
    ?tmpbuf
    ?(history_size=10)
    ?(heartbeat=Time_ns.Span.of_int_sec 30)
    ?(logon_fields=[])
    ~sid
    ~tid
    ~version uri =
  let closed = Ivar.create () in
  let do_logout = Ivar.create () in
  let do_cleanup = Ivar.create () in
  Clock_ns.every (Time_ns.Span.of_int_sec 60)
    Time_stamp_counter.Calibrator.calibrate
    ~stop:(Ivar.read do_cleanup)
    ~continue_on_error:false ;
  let logon =
    let fields =
      (Field.HeartBtInt.create
         (Time_ns.Span.to_int_sec heartbeat)) :: logon_fields
    in
    Fix.create ~fields Fixtypes.MsgType.Logon in
  let reject ?reason:_ ?text:_ rsn =
    let fields = [ Field.RefSeqNum.create rsn ] in
    Fix.create ~fields Fixtypes.MsgType.Reject in
  let history = ref (BoundedIntMap.empty history_size) in
  let last_received = ref (Time_stamp_counter.now ()) in
  let last_sent     = ref (Time_stamp_counter.now ()) in
  let count = ref 1 in
  let hb = Time_ns.Span.to_int63_ns heartbeat in
  with_connection ?tmpbuf uri >>= fun (r, w) ->
  let s msg =
    let msg = { msg with version ; seqnum = !count ; sid ; tid } in
    history := BoundedIntMap.add !history ~key:!count ~data:msg ;
    incr count ;
    last_sent := Time_stamp_counter.now () ;
    Pipe.write w msg in
  let watchdog () =
    let open Time_stamp_counter in
    let now = now () in
    let span_last_received = Span.to_ns (diff now !last_received) in
    let span_last_sent = Span.to_ns (diff now !last_sent) in
    let open Int63 in
    Logs_async.debug ~src begin fun m ->
      m "watchdog %a %a" pp span_last_received pp span_last_sent
    end >>= fun () ->
    if span_last_received > of_int 2 * hb then begin
      Ivar.fill_if_empty do_cleanup () ;
      Deferred.unit end
    else if span_last_received > hb then
      if span_last_sent > hb then
        s (Fix.heartbeat ())
      else
        s (Fix.create Fixtypes.MsgType.TestRequest)
    else
      Deferred.unit in
  s logon >>= fun () ->
  don't_wait_for begin
    Ivar.read do_logout >>= fun () ->
    s (Fix.create Fixtypes.MsgType.Logout)
  end ;
  Clock_ns.every' (Time_ns.Span.of_int_sec 5)
    watchdog
    ~continue_on_error:false
    ~stop:(Ivar.read do_cleanup) ;
  let r = Pipe.filter_map' r ~f:begin fun m ->
      last_received := Time_stamp_counter.now () ;
      match m.typ with
      | Logout when Ivar.is_full do_logout ->
        Ivar.fill_if_empty do_cleanup () ;
        return None
      | Logout ->
        s (Fix.create Fixtypes.MsgType.Logout) >>= fun () ->
        Ivar.fill_if_empty do_cleanup () ;
        return None
      | Heartbeat
      | TestRequest ->
        let testReqID = Field.(find_set TestReqID m.fields) in
        s (Fix.heartbeat ?testReqID ~sid ~tid ()) >>= fun () ->
        return None
      | ResendRequest ->
        (* TODO implement *)
        let bsn = Field.(find_set BeginSeqNo m.fields) in
        let esn = Field.(find_set EndSeqNo m.fields) in
        begin match bsn, esn with
        | Some _, Some _ -> s (reject m.seqnum)
        | _ -> s (reject m.seqnum)
        end >>= fun () ->
        return None
      | _ -> return (Some m)
    end in
  let rr, ww = Pipe.create () in
  don't_wait_for (Pipe.iter rr ~f:s) ;
  let cleanup () =
    Ivar.read do_cleanup >>= fun () ->
    Logs_async.warn
      (fun m -> m "with_connection_ez: cleaning up") >>| fun () ->
    Pipe.close w ;
    Pipe.close_read rr ;
    Ivar.fill closed () in
  don't_wait_for (cleanup ()) ;
  don't_wait_for (Pipe.closed ww >>| Ivar.fill_if_empty do_logout) ;
  return (Ivar.read closed, r, ww)
