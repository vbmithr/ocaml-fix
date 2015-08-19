open Core.Std
open Async.Std
open Async_ssl.Std

open Fix
open Fix_intf
open Fix_async
open Coinbase

let log = Log.(create ~level:`Debug ~output:[Output.(stderr ())]
                 ~on_error:`Raise)

let history = ref Int.Map.empty

let send_msg w mk_msg =
  let seqnum, msg = mk_msg () in
  history := Int.Map.add !history ~key:seqnum ~data:msg;
  Pipe.write w msg

let main () =
  let host = Sys.getenv_exn "COINBASE_HOST" in
  let port = int_of_string @@ Sys.getenv_exn "COINBASE_PORT" in
  let apikey = Sys.getenv_exn "COINBASE_APIKEY" in
  let apisecret = Sys.getenv_exn "COINBASE_APISECRET" in
  with_connection
    ~ssl:false ~host ~port ~username:apikey
    ~passwd:apisecret () >>= fun (r, w) ->
  Log.info log "Welcome to the Coinbase shell";
  let rec drain_input () =
    Pipe.read r >>= function
    | `Eof -> Deferred.unit
    | `Ok msg ->
      let msgtype = IntMap.find (tag_to_enum MsgType) msg |>
                    msgname_of_string in
      (match msgtype with
       | Some Logout ->
         (* Immediately send a logout msg and exit. *)
         send_msg w @@ (fun () -> logout apikey) >>= fun () ->
         Shutdown.exit 0
       | Some TestRequest ->
         (try
            let testreqid = IntMap.find (tag_to_enum TestReqID) msg in
            (* Immediately send a heartbeat with the same seqnum. *)
            send_msg w (fun () -> heartbeat ~testreqid apikey)
          with Not_found -> Deferred.unit)
       | Some ResendRequest -> Deferred.unit
       | _ -> Deferred.unit
      ) >>= fun () ->
      drain_input () in
  don't_wait_for @@ drain_input ();
  let rec heartbeat_loop w period =
    after @@ Time.Span.of_string (string_of_int period ^ "s") >>= fun () ->
    send_msg w (fun () -> heartbeat apikey) >>= fun () ->
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
         send_msg w (logon ~apikey ~apisecret) >>= fun () ->
         read_loop ()
       | "LOGOUT" ->
         send_msg w @@ (fun () -> logout apikey) >>= fun () ->
         read_loop ()
       | "TESTREQ" ->
         send_msg w
           (fun () -> testreq ~testreqid:Uuid.(create () |> to_string) apikey)
         >>= fun () -> read_loop ()
       (* | "SUB" -> *)
       (*   (match List.nth words 1 with *)
       (*   | None -> *)
       (*     Log.info log "SUB should be followed by either XBTUSD or LTCUSD"; *)
       (*     read_loop () *)
       (*   | Some curr -> *)
       (*     let symbol = match curr with *)
       (*       | "XBTUSD"|"BTCUSD" -> `XBTUSD *)
       (*       | "LTCUSD" -> `LTCUSD *)
       (*       | _ -> `XBTUSD in *)
       (*     let req_id = Uuid.(create () |> to_string) in *)
       (*     send_msg w (fun () -> incremental_trades ~symbol req_id) >>= fun () -> *)
       (*     read_loop () *)
       (*   ) *)
       (* | "ACCINFO" -> *)
       (*   let uuid_str = Uuid.(create () |> to_string) in *)
       (*   send_msg w (fun () -> account_info_request uuid_str) >>= fun () -> *)
       (*   read_loop () *)
       | command ->
         Log.info log "Unsupported command: %s" command;
         read_loop ()
      )
  in
  don't_wait_for @@ read_loop ();
  Pipe.closed w >>= fun () ->
  Shutdown.exit 0

let () =
  don't_wait_for @@ main ();
  never_returns @@ Scheduler.go ()
