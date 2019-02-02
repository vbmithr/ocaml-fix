open Rresult
open Core
open Async

open Bs_devkit
open Fix

let src = Logs.Src.create "fix.async"

let with_connection
  ?(tmpbuf=Bytes.create 4096)
  ~version uri =
  let client_read, msg_write = Pipe.create () in
  let msg_read, client_write = Pipe.create () in
  let run (r, w) =
    let handle_chunk msgbuf ~pos ~len =
      if len > Bytes.length tmpbuf then
        failwith "Message bigger than tmpbuf" ;
      Bigstring.To_bytes.blit msgbuf pos tmpbuf 0 len;
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
      Fix.to_bytes ~version msg
    end ;
    Reader.read_one_chunk_at_a_time r handle_chunk
  in
  don't_wait_for begin
    addr_of_uri uri >>= fun addr ->
    Monitor.try_with_or_error begin fun () ->
      Conduit_async.V2.connect addr >>=
      run >>= fun _ ->
      (* TODO: cases *)
      Deferred.unit
    end >>= function
    | Error e ->
      Logs_async.err ~src (fun m -> m "%a" Error.pp e)
    | Ok _ ->
      Logs_async.info ~src
        (fun m -> m "TCP connection terminated") >>= fun () ->
      Pipe.close client_write ;
      Deferred.unit
    end;
  return (client_read, client_write)

module BoundedIntMap : sig
  type 'a t

  val empty : int -> 'a t
  val add : 'a t -> key:int -> data:'a -> 'a t
  val find : 'a t -> int -> 'a option
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

  let find { m ; _ } k = Int.Map.find m k
end

let send_msg history =
  let count = ref 1 in
  fun w msg ->
    let seqnum = Field.MsgSeqNum.create !count in
    let msg = { msg with fields = seqnum :: msg.fields } in
    history := BoundedIntMap.add !history !count msg ;
    incr count ;
    Pipe.write w msg

let with_connection_ez
    ?tmpbuf
    ?(history_size=10)
    ?(heartbeat=Time_ns.Span.of_int_sec 30)
    ?(logon_fields=[])
    ~senderCompID
    ~targetCompID
    ~version uri =
  let base_fields =
    Field.[ SenderCompID.create senderCompID ;
            TargetCompID.create targetCompID ] in
  let logon =
    let fields =
      List.rev_append logon_fields
        (Field.HeartBtInt.create
           (Time_ns.Span.to_int_sec heartbeat) :: base_fields)
    in
    Fix.create ~typ:Fixtypes.MsgType.Logon ~fields in
  let heartbeat testreqid =
    let fields =
      match testreqid with
      | None -> base_fields
      | Some s -> Field.TestReqID.create s :: base_fields
    in
    Fix.create ~typ:Fixtypes.MsgType.Heartbeat ~fields in
  let reject ?reason ?test rsn =
    let fields = base_fields in
    Fix.create ~typ:Fixtypes.MsgType.Reject ~fields in
  let history = ref (BoundedIntMap.empty history_size) in
  let last_received = ref (Time_stamp_counter.now ()) in
  let last_send     = ref (Time_stamp_counter.now ()) in
  let s = send_msg history in
  with_connection ?tmpbuf ~version uri >>= fun (r, w) ->
  s w logon >>= fun () ->
  let r = Pipe.filter_map' r ~f:begin fun m ->
      last_received := Time_stamp_counter.now () ;
      match m.typ with
      | Heartbeat
      | TestRequest ->
        let treqid = Field.(find_list TestReqID m.fields) in
        s w (heartbeat treqid) >>= fun () ->
        return None
      | ResendRequest ->
        let bsn = Field.(find_list BeginSeqNo m.fields) in
        let esn = Field.(find_list EndSeqNo m.fields) in
        begin match bsn, esn with
        | Some _, Some _ -> ()
        | _ -> ()
        end ;
        return None
      | _ -> return (Some m)
    end in
  let rr, ww = Pipe.create () in
  don't_wait_for begin
    Pipe.transfer rr w ~f:begin fun m ->
      last_send := Time_stamp_counter.now () ;
      m
    end
  end ;
  return (r, ww)
