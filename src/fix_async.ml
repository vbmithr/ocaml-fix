(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult
open Core
open Async

open Fix

let src = Logs.Src.create "fix.async"

let write_iovec w iovec =
  List.fold_left iovec ~init:0 ~f:begin fun a { Faraday.buffer ; off ; len } ->
    Writer.write_bigstring w buffer ~pos:off ~len ;
    a+len
  end

let connect uri =
  let client_read, msg_write = Pipe.create () in
  let msg_read, client_write = Pipe.create () in
  let cleanup () =
      Logs_async.warn
        (fun m -> m "with_connection: cleanup up") >>| fun () ->
      Pipe.close_read msg_read ;
      Pipe.close msg_write in
  don't_wait_for (Pipe.closed client_write >>= cleanup) ;
  let stream = Faraday.create 4096 in
  let run _ r w =
    let rec flush () =
      match Faraday.operation stream with
      | `Close -> raise Exit
      | `Yield -> Deferred.unit
      | `Writev iovecs ->
        let nb_written = write_iovec w iovecs  in
        Faraday.shift stream nb_written ;
        flush ()
    in
    don't_wait_for begin
      Pipe.iter msg_read ~f:begin fun msg ->
        Fix.serialize stream msg ;
        flush () >>= fun () ->
        Logs_async.debug ~src (fun m -> m "-> %a" Fix.pp msg)
      end
    end ;
    let fields = ref [] in
    Angstrom_async.parse_many Field.parser begin fun field ->
      match field with
      | Error _ as e -> R.failwith_error_msg e
      | Ok ((field, _) as e) ->
        match Field.(find CheckSum field) with
        | None ->
          fields := e :: !fields ;
          Deferred.unit
        | Some chk ->
          match int_of_string_opt chk with
          | None -> failwith "checksum is not an integer"
          | Some chk ->
            let chk' =
              List.fold_left !fields ~init:0 ~f:(fun a (_, c) -> a + c) in
            if chk <> (chk' mod 256) then failwith "checksum failed"
            else
              match Fix.of_fields (List.rev_map !fields ~f:fst) with
              | Error _ as e -> R.failwith_error_msg e
              | Ok msg ->
                fields := [] ;
                Logs_async.debug (fun m -> m "<- %a" pp msg) >>= fun () ->
                Pipe.write msg_write msg
    end r >>= function
    | Error msg -> failwith msg
    | Ok () -> Deferred.unit
  in
  don't_wait_for begin
    Monitor.try_with ~extract_exn:true
      (fun () -> Conduit_async.V3.with_connection_uri uri run) >>= function
    | Error Exit ->
      Logs_async.err ~src (fun m -> m "Serializer closed") >>=
      cleanup
    | Error e ->
      Logs_async.err ~src (fun m -> m "%a" Exn.pp e) >>=
      cleanup
    | Ok _ ->
      Logs_async.info ~src
        (fun m -> m "TCP connection terminated") >>=
      cleanup
  end;
  return (client_read, client_write)

let with_connection url ~f =
  connect url >>= fun (r, w) ->
  Monitor.protect (fun () -> f r w)
    ~finally:begin fun () ->
      Pipe.close w ; Pipe.close_read r ; Deferred.unit
    end

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

let connect_ez
    ?(history_size=10)
    ?(heartbeat=Time_ns.Span.of_int_sec 30)
    ?(logon_fields=[])
    ?logon_ts
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
  connect uri >>= fun (r, w) ->
  let s msg =
    last_sent := Time_stamp_counter.now () ;
    let ts = begin
      match msg.typ, logon_ts with
      | Logon, Some ts -> Some ts
      | _, Some _ ->
        let open Int63 in
        Time_ns.to_int63_ns_since_epoch
          (Time_stamp_counter.to_time_ns !last_sent) / of_int 1_000_000_000 |> to_float |>
        Ptime.of_float_s
      | _, None -> None
    end in
    let msg = { msg with version ; seqnum = !count ; sid ; tid ; ts } in
    history := BoundedIntMap.add !history ~key:!count ~data:msg ;
    incr count ;
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

let with_connection_ez
    ?history_size ?heartbeat ?logon_fields
    ?logon_ts ~sid ~tid ~version uri ~f =
  connect_ez ?history_size ?heartbeat ?logon_fields
    ?logon_ts ~sid ~tid ~version uri >>= fun (closed, r, w) ->
  Monitor.protect (fun () -> f ~closed r w)
    ~finally:begin fun () ->
      Pipe.close w ; Pipe.close_read r ; Deferred.unit
    end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
