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
  }
  module Address = struct
    include Uri_sexp
    let equal = Uri.equal
  end

  let create r w = { r; w }

  let is_closed { r; w } =
    Pipe.(is_closed r && is_closed w)

  let close { r; w } =
    Pipe.close w ;
    Pipe.close_read r ;
    Deferred.unit

  let close_finished { r; w } =
    Deferred.all_unit [Pipe.closed r ; Pipe.closed w]
end
include T

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
  condition: status Condition.t ;
  mutable status: status ;
  mutable history: Fix.t BoundedIntMap.t ;
  mutable last_received: Time_ns.t ;
  mutable last_sent: Time_ns.t ;
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
  condition = Condition.create () ;
  status = `Connected ;
  history = BoundedIntMap.empty history_size ;
  last_received = Time_ns.epoch ;
  last_sent = Time_ns.epoch ;
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
    (({ last_sent; count;
        logon_ts; w; version; sid; tid; status; _ } as st)) msg =
  match status, msg.typ with
  | `Connected, Logon
  | `Closing, Logout
  | `Authenticated, _ ->
    st.last_sent <- Time_ns.now () ;
    let ts = begin
      match msg.typ, logon_ts with
      | Logon, Some ts -> Some ts
      | _, None -> None
      | _, Some _ -> ptime_of_time_ns last_sent
    end in
    let msg = { msg with version; sid; tid; seqnum = count ; ts } in
    st.history <- BoundedIntMap.add st.history ~key:count ~data:msg ;
    st.count <- succ st.count ;
    Pipe.write w msg
  | _ -> Deferred.unit

let mk_client_write ~monitor st =
  Pipe.create_writer begin fun r ->
    Scheduler.within' ~monitor
      (fun () -> Pipe.iter r ~f:(fun m -> send_msg st m))
  end

let filter_messages st w m =
  st.last_received <- Time_ns.now () ;
  match m.typ, st.status with
  | Logon, `Connected ->
    st.status <- `Authenticated ;
    Condition.broadcast st.condition `Authenticated ;
    Pipe.write w m
  | Logout, `Closing -> (* End of connection, cleaning up *)
    Log_async.debug (fun m -> m "Logout response received") >>= fun () ->
    Pipe.close w ;
    Deferred.unit
  | Reject, _
  | Logout, _ ->
    Log_async.debug (fun m -> m "Logout request received") >>= fun () ->
    send_msg st (Fix.create Fixtypes.MsgType.Logout) >>= fun _ ->
    st.status <- `Closed ;
    Pipe.close w ;
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
    Pipe.write w m
  | _ -> assert false

let mk_client_read ~monitor st =
  Pipe.create_reader ~close_on_exception:true begin fun w ->
    Scheduler.within' ~monitor
      (fun () -> Pipe.iter st.r ~f:(filter_messages st w))
  end

let watchdog ({ last_received; last_sent; heartbeat; status; _ } as st) =
  match status with
  | `Connected
  | `Closing
  | `Closed -> Deferred.unit
  | `Authenticated ->
    let now = Time_ns.now () in
    let span_last_received = Time_ns.diff now last_received in
    let span_last_sent = Time_ns.diff now last_sent in
    Log_async.debug begin fun m ->
      m "watchdog %a %a"
        Time_ns.Span.pp span_last_received
        Time_ns.Span.pp span_last_sent
    end >>= fun () ->
    if Time_ns.Span.(span_last_received > scale_int heartbeat 2) then begin
      (* close connection *)
      Pipe.close_read st.r ;
      Pipe.close st.w ;
      Deferred.unit
    end
    else if Time_ns.Span.(span_last_received > heartbeat) then
      if Time_ns.Span.(span_last_sent > heartbeat) then
        send_msg st (Fix.heartbeat ())
      else
        send_msg st (Fix.create Fixtypes.MsgType.TestRequest)
    else
      Deferred.unit

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
  send_msg st (logon st) >>= fun () ->
  let stop =
    Deferred.all_unit [Pipe.closed r; Pipe.closed w] in
  Clock_ns.every' ~continue_on_error:false ~stop
    (Time_ns.Span.of_int_sec 5) (fun () -> watchdog st) ;
  let monitor = Monitor.create () in
  let client_read = mk_client_read ~monitor st in
  let client_write = mk_client_write ~monitor st in
  don't_wait_for begin
    Deferred.all_unit [Pipe.closed client_read;
                       Pipe.closed client_write] >>= fun () ->
    match st.status with
    | `Closing | `Closed -> Deferred.unit
    | `Authenticated ->
      Log_async.warn (fun m -> m "connect: closing") >>= fun () ->
      st.status <- `Closing ;
      send_msg st (Fix.create Fixtypes.MsgType.Logout) >>= fun _ ->
      Deferred.unit
    | `Connected ->
      Condition.wait st.condition >>= fun _ ->
      Log_async.warn (fun m -> m "connect: closing") >>= fun () ->
      st.status <- `Closing ;
      send_msg st (Fix.create Fixtypes.MsgType.Logout) >>= fun _ ->
      Deferred.unit
  end ;
  let log_exn exn = Log.err (fun m -> m "%a" Exn.pp exn) in
  Monitor.detach_and_iter_errors monitor ~f:log_exn ;
  return (create client_read client_write)

let with_connection
    ?history_size ?heartbeat ?logon_fields
    ?logon_ts ~sid ~tid ~version uri ~f =
  connect ?history_size ?heartbeat ?logon_fields
    ?logon_ts ~sid ~tid ~version uri >>= fun { r; w } ->
  Monitor.protect (fun () -> f r w)
    ~finally:begin fun () ->
      Pipe.close w ; Pipe.close_read r ; Deferred.unit
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create'
      ~server_name ?history_size ?heartbeat ?logon_fields
      ?logon_ts ?on_event ?retry_delay ~sid ~tid ~version =
    let connect url =
      connect ?history_size ?heartbeat
        ?logon_fields ?logon_ts ~sid ~tid ~version url >>=
      Deferred.Or_error.return in
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
