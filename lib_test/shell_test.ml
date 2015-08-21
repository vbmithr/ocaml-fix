open Core.Std
open Async.Std

open Mt
open Fix
open Fix_intf
open Fix_async
open Testserver

let log = Log.(create ~level:`Debug ~output:[Output.(stderr ())]
                 ~on_error:`Raise)

let history = ref Int.Map.empty

let send_msg w mk_msg =
  let seqnum, msg = mk_msg () in
  history := Int.Map.add !history ~key:seqnum ~data:msg;
  Pipe.write w msg

let main host port =
  with_connection ~ssl:false ~host ~port () >>= fun (r, w) ->
  Log.info log "Connected to %s %d" host port;
  let rec drain_input () =
    Pipe.read r >>= function
    | `Eof -> Deferred.unit
    | `Ok msg ->
      let msgtype =
        Option.bind (Msg.find msg (tag_to_enum MsgType))
          msgname_of_string in
      (match msgtype with
       | Some Logout ->
         (* Immediately send a logout msg and exit. *)
         send_msg w logout >>= fun () ->
         Shutdown.exit 0
       | Some TestRequest ->
         (Msg.find msg (tag_to_enum TestReqID) |> function
         | None -> Deferred.unit
         | Some testreqid ->
           (* Immediately send a heartbeat with the same seqnum. *)
           send_msg w (heartbeat ~testreqid))
       | Some ResendRequest -> Deferred.unit
       | _ -> Deferred.unit
      ) >>= fun () ->
      drain_input () in
  don't_wait_for @@ drain_input ();
  let rec heartbeat_loop w period =
    after @@ Time.Span.of_string (string_of_int period ^ "s") >>= fun () ->
    send_msg w heartbeat >>= fun () ->
    heartbeat_loop w period
  in
  don't_wait_for @@ heartbeat_loop w 30;
  let rec read_loop () =
    Reader.(read_line Lazy.(force stdin)) >>= function
    | `Eof ->
      Log.info log "EOF received, exiting.";
      Shutdown.exit 0
    | `Ok msg ->
      let words = String.split msg ~on:' ' in
      (match List.hd_exn words with
       | "LOGON" ->
         send_msg w logon >>= fun () ->
         read_loop ()
       | "LOGOUT" ->
         send_msg w logout >>= fun () ->
         read_loop ()
       | "TESTREQ" ->
         send_msg w
           (fun () -> testreq @@ Uuid.(create () |> to_string))
         >>= fun () -> read_loop ()
       (* | "BUY" -> *)
       (*   let symbol = Option.value_exn *)
       (*       (List.nth_exn words 1 |> Symbol.of_string) in *)
       (*   let p = List.nth_exn words 2 |> Float.of_string in *)
       (*   let v = List.nth_exn words 3 |> Float.of_string in *)
       (*   send_msg w *)
       (*     (new_order *)
       (*         ~uuid:Uuid.(create () |> to_string) *)
       (*         ~symbol ~p ~v ~direction:`Buy) *)
       (*     >>= fun () -> read_loop () *)
       (* | "SELL" -> *)
       (*   let symbol = Option.value_exn *)
       (*       (List.nth_exn words 1 |> Symbol.of_string) in *)
       (*   let p = List.nth_exn words 2 |> Float.of_string in *)
       (*   let v = List.nth_exn words 3 |> Float.of_string in *)
       (*   send_msg w *)
       (*     (new_order *)
       (*         ~uuid:Uuid.(create () |> to_string) *)
       (*         ~symbol ~p ~v ~direction:`Sell) *)
       (*     >>= fun () -> read_loop () *)
       | command ->
         Log.info log "Unsupported command: %s" command;
         read_loop ()
      )
  in
  Signal.(handle terminating ~f:(fun _ ->
      don't_wait_for @@ send_msg w logout));
  don't_wait_for @@ read_loop ();
  Pipe.closed w >>= fun () ->
  Shutdown.exit 0

let () =
  if Array.length Sys.argv < 3 then
    (Printf.eprintf "Usage: %s host port\n" Sys.argv.(0);
     Pervasives.exit 1
    )
  else
    let host = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
    don't_wait_for @@ main host port;
  never_returns @@ Scheduler.go ()
