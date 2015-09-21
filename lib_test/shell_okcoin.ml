open Core.Std
open Async.Std

open Fix
open Fix_intf
open Fix_async
open Okcoin

let log = Log.(create ~level:`Debug ~output:[Output.(stderr ())]
                 ~on_error:`Raise)

let history = ref Int.Map.empty

let send_msg w mk_msg =
  let seqnum, msg = mk_msg () in
  history := Int.Map.add !history ~key:seqnum ~data:msg;
  Pipe.write w msg

let main () =
  let host = Sys.getenv_exn "OKCOIN_HOST" in
  let port = int_of_string @@ Sys.getenv_exn "OKCOIN_PORT" in
  let username = Sys.getenv_exn "OKCOIN_USER" in
  let passwd = Sys.getenv_exn "OKCOIN_PASSWD" in
  with_connection ~tls:true ~host ~port () >>= fun (r, w) ->
  Log.info log "Connected to OKCoin";
  let rec drain_input () =
    Pipe.read r >>= function
    | `Eof -> Deferred.unit
    | `Ok msg ->
      let msgtype = Option.bind
          (Msg.find msg (tag_to_enum MsgType))
          msgname_of_string in
      (match msgtype with
       | Some Logout ->
         (* Immediately send a logout msg and exit. *)
         send_msg w logout >>= fun () ->
         Shutdown.exit 0
       | Some TestRequest ->
         (Msg.find msg (tag_to_enum TestReqID) |> function
         | None -> Deferred.unit
         (* Immediately send a heartbeat with the same seqnum. *)
         | Some testreqid ->
           send_msg w (heartbeat ~testreqid ~username ~passwd)
         )
       | Some ResendRequest ->
         (* Send the required messages. *)
         let from = Option.value_exn (Msg.find msg 7) |> int_of_string in
         let to_ = Option.value_exn (Msg.find msg 16) |> int_of_string in
         Int.Map.iter !history ~f:(fun ~key ~data ->
             if key >= from && (to_ = 0 || key <= to_)
             then don't_wait_for @@ send_msg w (fun () -> key, data)
           );
         Deferred.unit
       | _ -> Deferred.unit
      ) >>= fun () ->
      drain_input () in
  don't_wait_for @@ drain_input ();
  let rec heartbeat_loop w period =
    after @@ Time.Span.of_string (string_of_int period ^ "s") >>= fun () ->
    send_msg w (heartbeat ~username ~passwd) >>= fun () ->
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
      (match String.uppercase @@ List.hd_exn words with
       | "LOGON" ->
         send_msg w (logon ~username ~passwd) >>=
         read_loop
       | "LOGOUT" ->
         send_msg w @@ logout ~response:true >>=
         read_loop
       | "TESTREQ" ->
         send_msg w (fun () -> testreq Uuid.(create () |> to_string)) >>=
         read_loop
       | "SUB" ->
         (match List.nth words 1 with
         | None ->
           Log.info log "SUB should be followed by either XBTUSD or LTCUSD";
           read_loop ()
         | Some curr ->
           let symbol = match curr with
             | "XBTUSD"|"BTCUSD" -> `XBTUSD
             | "LTCUSD" -> `LTCUSD
             | _ -> `XBTUSD in
           let req_id = Uuid.(create () |> to_string) in
           send_msg w (fun () -> incremental_trades ~symbol req_id) >>=
           read_loop
         )
       | "ACCINFO" ->
         let uuid_str = Uuid.(create () |> to_string) in
         send_msg w (fun () -> account_info_request uuid_str) >>=
         read_loop
       | "ORDINFO" ->
         (List.nth words 1 |> function
         | None ->
           Log.info log "Please specify an order id";
           read_loop ()
         | Some start_id ->
           let reqid = Uuid.(create () |> to_string) in
           send_msg w (fun () ->
               orders_request ~start_id ~status:`Not_filled
                 ~symbol:`XBTUSD reqid
             ) >>= read_loop
         )
       | "EXIT" -> Shutdown.exit 0
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
  don't_wait_for @@ main ();
  never_returns @@ Scheduler.go ()
