open Core.Std
open Async.Std
open Log.Global

open Bs_devkit.Core
open Coinbase

let history = ref Int.Map.empty

let host = "fix.gdax.com"
let port = 4198

let send_msg w mk_msg =
  let seqnum, msg = mk_msg () in
  history := Int.Map.add !history ~key:seqnum ~data:msg;
  Pipe.write w msg

let rec process_input r w =
  Pipe.read r >>= function
  | `Eof -> Deferred.unit
  | `Ok msg ->
    begin match msg.Fix.typ with
      | Logout ->
        (* Immediately send a logout msg and exit. *)
        send_msg w logout >>= fun () ->
        Shutdown.exit 0
      | TestRequest -> begin match Fix.Tag.Map.find Fix.Tag.(S TestReqID) msg.fields with
          | exception Not_found -> Deferred.unit
          | testreqid ->
            (* Immediately send a heartbeat with the same seqnum. *)
            send_msg w (heartbeat ~testreqid)
        end
      | ResendRequest -> Deferred.unit
      | _ -> Deferred.unit
    end >>= fun () ->
    process_input r w

let rec heartbeat_loop w period =
  after @@ Time.Span.of_string (string_of_int period ^ "s") >>= fun () ->
  send_msg w heartbeat >>= fun () ->
  heartbeat_loop w period

let process_msg ~secret ~passphrase w words = match List.hd_exn words with
  | "LOGON" -> send_msg w (logon ~secret ~passphrase)
  | "LOGOUT" -> send_msg w logout
  | "TESTREQ" -> send_msg w (fun () -> testreq @@ Uuid.(create () |> to_string))
  | "BUY" ->
    let symbol = List.nth_exn words 1 in
    let p = List.nth_exn words 2 |> Float.of_string in
    let v = List.nth_exn words 3 |> Float.of_string in
    send_msg w
      (new_order
         ~uuid:Uuid.(create () |> to_string)
         ~symbol ~p ~v ~side:Buy)
  | "SELL" ->
    let symbol = List.nth_exn words 1 in
    let p = List.nth_exn words 2 |> Float.of_string in
    let v = List.nth_exn words 3 |> Float.of_string in
    send_msg w
      (new_order
         ~uuid:Uuid.(create () |> to_string)
         ~symbol ~p ~v ~side:Sell)
  | command ->
    info "Unsupported command: %s" command;
    Deferred.unit

let rec process_user_input ~secret ~passphrase w =
  let rec loop () =
    Reader.(read_line Lazy.(force stdin)) >>= function
    | `Eof ->
      info "EOF received, exiting.";
      Shutdown.exit 0
    | `Ok msg ->
      process_msg ~secret ~passphrase w @@ String.split msg ~on:' ' >>=
      loop
  in loop ()

let main cfg ca_file loglevel () =
  Option.iter loglevel ~f:(Fn.compose set_level loglevel_of_int);
  let cfg = Yojson.Safe.from_file cfg |> Cfg.of_yojson |> Result.ok_or_failwith in
  let { Cfg.key; secret; passphrase } = List.Assoc.find_exn cfg "GDAX" in
  init key;
  let run () =
    Fix_async.with_connection ~log:(Lazy.force log) ~tls:(`CAFile ca_file) ~host ~port () >>= fun (r, w) ->
    Signal.(handle terminating ~f:(fun _ -> don't_wait_for @@ send_msg w logout));
    info "Connected to Coinbase";
    don't_wait_for @@ process_input r w;
    don't_wait_for @@ heartbeat_loop w 30;
    don't_wait_for @@ process_user_input ~secret ~passphrase w;
    Pipe.closed w >>= fun () ->
    Shutdown.exit 0
  in
  don't_wait_for @@ run ();
  never_returns @@ Scheduler.go ()

let command =
  let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu" in
  let spec =
    let open Command.Spec in
    empty
    +> flag "-cfg" (optional_with_default default_cfg string) ~doc:"path Filepath of config file (default: ~/.virtu)"
    +> flag "-ca-file" (optional_with_default "fix.gdax.com.pem" string) ~doc:"path Filepath of CA certificate (default: fix.gdax.com.pem)"
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"

  in
  Command.basic ~summary:"Coinbase test shell" spec main

let () = Command.run command
