open Core.Std
open Async.Std
open Async_ssl.Std

open Fix
open Fix_async
open Okcoin

let log = Log.(create ~level:`Debug ~output:[Output.(stderr ())]
                 ~on_error:`Raise)

let history = ref Int.Map.empty

let main () =
  let host = Sys.getenv_exn "OKCOIN_HOST" in
  let port = int_of_string @@ Sys.getenv_exn "OKCOIN_PORT" in
  let username = Sys.getenv_exn "OKCOIN_USER" in
  let passwd = Sys.getenv_exn "OKCOIN_PASSWD" in
  with_connection ~ssl:false ~host ~port ~username ~passwd () >>= fun (r, w) ->
  Log.info log "Welcome to the OKCoin shell";
  let rec drain_input () =
    Pipe.read r >>= function
    | `Eof -> Deferred.unit
    | `Ok msg -> drain_input () in
  don't_wait_for @@ drain_input ();
  let send_msg w mk_msg =
    let seqnum, msg = mk_msg () in
    history := Int.Map.add !history ~key:seqnum ~data:msg;
    Pipe.write w msg
  in
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
      (match msg with
       | "LOGON" ->
         send_msg w (logon ~username ~passwd) >>= fun () ->
         read_loop ()
       | "LOGOUT" ->
         send_msg w logout >>= fun () ->
         read_loop ()
       | "TESTREQ" ->
         send_msg w (fun () -> testreq @@ Random.int 100) >>= fun () ->
         read_loop ()
       | command ->
         Log.info log "Unsupported command: %s" command;
         read_loop ()
      )
  in
  read_loop ()

let () =
  don't_wait_for @@ main ();
  never_returns @@ Scheduler.go ()
