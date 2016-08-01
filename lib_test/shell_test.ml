open Core.Std
open Async.Std
open Log.Global

let create = Fix.make_create ~sendercompid:"CLIENT1" ~targetcompid:"EXECUTOR" ()

let logon ?(heartbeat=30) () =
  let fields = Fix.Tag.[
      S EncryptMethod, "0"; (* encryption *)
      S HeartBtInt, string_of_int heartbeat;
      (* 553, username; *)
      (* 554, passwd; *)
    ]
  in
  create Logon fields

let logout ?(response=false) () =
  create Logout (if response then [C 8500, "0"] else [])

let heartbeat ?testreqid () =
  let fields =
    (match testreqid with
     | None -> []
     | Some value -> Fix.Tag.[S TestReqID, value])
  in
  create Heartbeat fields

let testreq reqid = create TestRequest Fix.Tag.[S TestReqID, reqid]

let history = ref Int.Map.empty

let send_msg w mk_msg =
  let seqnum, msg = mk_msg () in
  history := Int.Map.add !history ~key:seqnum ~data:msg;
  Pipe.write w msg

let on_incoming_msg w msg = match msg.Fix.typ with
  | Logout ->
    (* Immediately send a logout msg and exit. *)
    send_msg w logout >>= fun () ->
    Shutdown.exit 0
  | TestRequest -> begin
      match Fix.Tag.Map.find (Fix.Tag.S TestReqID) msg.fields with
      | exception Not_found -> Deferred.unit
      | testreqid ->
        (* Immediately send a heartbeat with the same seqnum. *)
        send_msg w (heartbeat ~testreqid)
    end
  | ResendRequest -> Deferred.unit
  | _ -> Deferred.unit

let rec heartbeat_loop w period =
  after @@ Time.Span.of_string (string_of_int period ^ "s") >>= fun () ->
  send_msg w heartbeat >>= fun () ->
  heartbeat_loop w period

let on_user_cmd w msg =
  match List.hd_exn @@ String.split msg ~on:' ' with
  | "LOGON" -> send_msg w logon
  | "LOGOUT" -> send_msg w logout
  | "TESTREQ" -> send_msg w (fun () -> testreq @@ Uuid.(create () |> to_string))
  | command ->
    info "Unsupported command: %s" command;
    Deferred.unit

let main host port =
  Fix_async.with_connection ~tls:`Noconf ~host ~port () >>= fun (r, w) ->
  info "Connected to %s %d" host port;
  Signal.(handle terminating ~f:(fun _ -> don't_wait_for @@ send_msg w logout));
  Deferred.any [
    Pipe.iter Reader.(stdin |> Lazy.force |> pipe) ~f:(on_user_cmd w);
    Pipe.iter r ~f:(on_incoming_msg w);
    heartbeat_loop w 30;
    Pipe.closed w;
  ] >>= fun () ->
  Shutdown.exit 0

let main (host, port) () =
  don't_wait_for @@ main host port;
  never_returns @@ Scheduler.go ()

let command =
  let spec =
    let open Command.Spec in
    empty
    +> anon (t2 ("host" %: string) ("port" %: int))
  in
  Command.basic ~summary:"FIX test shell" spec main

let () = Command.run command
