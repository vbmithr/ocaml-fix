(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async
open Fix
open Fix_async_raw

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

module T = struct
  type t = {
    r: Fix.t Pipe.Reader.t ;
    w: Fix.t Pipe.Writer.t ;
    closed: unit Deferred.t ;
  }
  module Address = struct
    include Uri_sexp
    let equal = Uri.equal
  end

  let is_closed { r; _ } = Pipe.is_closed r
  let close { r; w; closed } =
    Pipe.close w ; Pipe.close_read r ; closed
  let close_finished { closed; _ } = closed
end
include T

let create r w closed = { r; w; closed }

type st = {
  r : Fix.t Pipe.Reader.t ;
  w : Fix.t Pipe.Writer.t ;
  heartbeat: Time_ns.Span.t ;
  sid: string;
  tid: string;
  version: Fixtypes.Version.t ;
  logon_fields: Fix.Field.t list;
  logon_ts:Ptime.t option;
  closed: unit Ivar.t ;
  do_cleanup: unit Ivar.t ;
  calibrator: Time_stamp_counter.Calibrator.t ;
  condition: status Condition.t ;
  mutable status: status ;
  mutable history: Fix.t BoundedIntMap.t ;
  mutable last_received: Time_stamp_counter.t ;
  mutable last_sent: Time_stamp_counter.t ;
  mutable count: int ;
}
and status = [
  | `Connected
  | `Authenticated
  | `Closing
  | `Closed
]

let create_st
    ?logon_ts ?(logon_fields=[])
    ?(heartbeat=Time_ns.Span.of_int_sec 30)
    ~history_size ~sid ~tid version r w = {
  r; w; heartbeat; logon_fields; logon_ts; sid; tid; version;
  closed = Ivar.create () ;
  do_cleanup = Ivar.create () ;
  condition = Condition.create () ;
  status = `Connected ;
  calibrator = Lazy.force Time_stamp_counter.calibrator ;
  history = BoundedIntMap.empty history_size ;
  last_received = Time_stamp_counter.now () ;
  last_sent = Time_stamp_counter.now () ;
  count = 1 ;
}

let reject ?reason:_ ?text:_ rsn =
  Fix.create ~fields:[ Field.RefSeqNum.create rsn ]
    Fixtypes.MsgType.Reject

let logon { heartbeat; logon_fields; _ } =
  let fields =
    (Field.HeartBtInt.create (Time_ns.Span.to_int_sec heartbeat)) ::
    logon_fields in
  Fix.create ~fields Fixtypes.MsgType.Logon

let ptime_of_time_ns t =
  let open Int63 in
  Time_ns.to_int63_ns_since_epoch t / of_int 1_000_000_000 |>
  to_float |> Ptime.of_float_s

let send_msg
    (({ calibrator; last_sent; count;
        logon_ts; w; version; sid; tid; status; _ } as st)) msg =
  match status, msg.typ with
  | `Connected, Logon
  | `Closing, Logout
  | `Authenticated, _ ->
    st.last_sent <- Time_stamp_counter.now () ;
    let ts = begin
      match msg.typ, logon_ts with
      | Logon, Some ts -> Some ts
      | _, None -> None
      | _, Some _ ->
        ptime_of_time_ns
          (Time_stamp_counter.to_time_ns ~calibrator last_sent)
    end in
    let msg = { msg with version; sid; tid; seqnum = count ; ts } in
    st.history <- BoundedIntMap.add st.history ~key:count ~data:msg ;
    st.count <- succ st.count ;
    Monitor.try_with_or_error (fun () -> Pipe.write w msg)
  | _ -> Deferred.Or_error.ok_unit

let filter_messages st process_w m =
  st.last_received <- Time_stamp_counter.now () ;
  match m.typ, st.status with
  | Logon, `Connected ->
    st.status <- `Authenticated ;
    Condition.broadcast st.condition `Authenticated ;
    Pipe.write_if_open process_w m
  | Logout, `Closing -> (* End of connection, cleaning up *)
    Log_async.debug (fun m -> m "Logout response received") >>= fun () ->
    Ivar.fill_if_empty st.do_cleanup () ;
    Deferred.unit
  | Logout, _ ->
    Log_async.debug (fun m -> m "Logout request received") >>= fun () ->
    send_msg st (Fix.create Fixtypes.MsgType.Logout) >>= fun _ ->
    st.status <- `Closed ;
    Ivar.fill_if_empty st.do_cleanup () ;
    Deferred.unit
  | _, `Closing
  | _, `Closed -> Deferred.unit
  | Heartbeat, `Authenticated
  | TestRequest, `Authenticated ->
    let testReqID = Field.(Set.find_typ TestReqID m.fields) in
    send_msg st (Fix.heartbeat ?testReqID ()) >>= fun _ ->
    Deferred.unit
  | ResendRequest, `Authenticated ->
    (* TODO implement *)
    let bsn = Field.(Set.find_typ BeginSeqNo m.fields) in
    let esn = Field.(Set.find_typ EndSeqNo m.fields) in
    begin match bsn, esn with
      | Some _, Some _ -> send_msg st (reject m.seqnum)
      | _ -> send_msg st (reject m.seqnum)
    end >>= fun _ ->
    Deferred.unit
  | _, `Authenticated ->
    Pipe.write_if_open process_w m
  | _ -> assert false

let watchdog ({ calibrator; last_received; last_sent;
                do_cleanup; heartbeat; status; _ } as st) =
  match status with
  | `Connected
  | `Closing
  | `Closed -> Deferred.Or_error.ok_unit
  | `Authenticated ->
    let hb = Time_ns.Span.to_int63_ns heartbeat in
    let now = Time_stamp_counter.now () in
    let span_last_received =
      Time_stamp_counter.Span.to_ns
        ~calibrator (Time_stamp_counter.diff now last_received) in
    let span_last_sent =
      Time_stamp_counter.Span.to_ns
        ~calibrator (Time_stamp_counter.diff now last_sent) in
    let open Int63 in
    Log_async.debug begin fun m ->
      m "watchdog %a %a" pp span_last_received pp span_last_sent
    end >>= fun () ->
    if span_last_received > of_int 2 * hb then begin
      Ivar.fill_if_empty do_cleanup () ;
      Deferred.Or_error.ok_unit
    end
    else if span_last_received > hb then
      if span_last_sent > hb then
        send_msg st (Fix.heartbeat ())
      else
        send_msg st (Fix.create Fixtypes.MsgType.TestRequest)
    else
      Deferred.Or_error.ok_unit

let start_calibration { calibrator; do_cleanup; _ } =
  Clock_ns.every (Time_ns.Span.of_int_sec 60)
    (fun () -> Time_stamp_counter.Calibrator.calibrate calibrator)
    ~stop:(Ivar.read do_cleanup)
    ~continue_on_error:false

let connect
    ?(history_size=10)
    ?heartbeat
    ?logon_fields
    ?logon_ts
    ~sid
    ~tid
    ~version uri =
  connect uri >>= fun (r, w) ->
  let st = create_st ?heartbeat ?logon_ts ?logon_fields
      ~history_size ~sid ~tid version r w in
  start_calibration st ;
  send_msg st (logon st) >>=? fun () ->
  Clock_ns.every' (Time_ns.Span.of_int_sec 5)
    (fun () -> Deferred.Or_error.ok_exn (watchdog st))
    ~continue_on_error:false
    ~stop:(Ivar.read st.do_cleanup) ;
  let process_r, client_w = Pipe.create () in
  let client_r, process_w = Pipe.create () in
  don't_wait_for
    (Pipe.iter r ~f:(filter_messages st process_w)) ;
  don't_wait_for
    (Pipe.iter process_r ~f:(fun m -> Deferred.Or_error.ok_exn (send_msg st m))) ;
  let cleanup () =
    Ivar.read st.do_cleanup >>= fun () ->
    Log_async.warn (fun m -> m "EZ.connect: cleaning up") >>| fun () ->
    Pipe.close client_w ;
    Pipe.close_read client_r ;
    Pipe.close w ;
    Pipe.close_read r ;
    Ivar.fill st.closed () in
  don't_wait_for (cleanup ()) ;
  don't_wait_for begin
    Deferred.all_unit [Pipe.closed client_r; Pipe.closed client_w] >>= fun () ->
    match st.status with
    | `Closing | `Closed -> Deferred.unit
    | `Authenticated ->
      Log_async.warn (fun m -> m "EZ.connect: closing") >>= fun () ->
      st.status <- `Closing ;
      send_msg st (Fix.create Fixtypes.MsgType.Logout) >>= fun _ ->
      Deferred.unit
    | `Connected ->
      Condition.wait st.condition >>= fun _ ->
      Log_async.warn (fun m -> m "EZ.connect: closing") >>= fun () ->
      st.status <- `Closing ;
      send_msg st (Fix.create Fixtypes.MsgType.Logout) >>= fun _ ->
      Deferred.unit
  end ;
  Deferred.Or_error.return (create client_r client_w (Ivar.read st.closed))

let with_connection
    ?history_size ?heartbeat ?logon_fields
    ?logon_ts ~sid ~tid ~version uri ~f =
  connect ?history_size ?heartbeat ?logon_fields
    ?logon_ts ~sid ~tid ~version uri >>=? fun { r; w; closed } ->
  Monitor.protect (fun () -> f ~closed r w)
    ~finally:begin fun () ->
      Pipe.close w ; Pipe.close_read r ; Deferred.unit
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create'
      ~server_name ?history_size ?heartbeat ?logon_fields
      ?logon_ts ?on_event ?retry_delay ~sid ~tid ~version =
    let connect url = connect ?history_size ?heartbeat ?logon_fields
        ?logon_ts ~sid ~tid ~version url in
    create ~server_name ?on_event ?retry_delay ~connect
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
