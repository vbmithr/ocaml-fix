open Core
open Async

open Bs_devkit
open Fix
open Fixtypes
open Deribit

let src = Logs.Src.create "fix.deribit.shell"
let uri = Uri.make ~host:"test.deribit.com" ~port:9881 ()

let logout = Fix.create MsgType.Logout
let hb msg =
  Fix.create ~fields:[Field.TestReqID.create msg] MsgType.Heartbeat

let on_server_msg w msg = match msg.Fix.typ with
  | Logout ->
    (* Immediately send a logout msg and exit. *)
    Pipe.write w logout >>= fun () ->
    Shutdown.exit 0
  | TestRequest -> begin
      match Field.find_list Field.TestReqID msg.fields with
      | None -> Deferred.unit
      | Some testreqid ->
        (* Immediately send a heartbeat with the same seqnum. *)
        Pipe.write w (hb testreqid)
    end
  | ResendRequest -> Deferred.unit
  | _ -> Deferred.unit

(* let rec heartbeat_loop w period =
 *   Clock_ns.(after (Time_ns.Span.of_int_sec period)) >>= fun () ->
 *   send_msg w (heartbeat "") >>= fun () ->
 *   heartbeat_loop w period *)

let on_client_cmd w words =
  let words = String.split ~on:' ' @@ String.chop_suffix_exn words ~suffix:"\n" in
  match String.lowercase (List.hd_exn words) with
  | "testreq" ->
    let fields = [Field.TestReqID.create "a"] in
    Pipe.write w (Fix.create ~fields MsgType.TestRequest)
  | command ->
    Logs_async.app ~src (fun m -> m "Unsupported command: %s" command)
  (* | "BUY" ->
   *   let symbol = List.nth_exn words 1 in
   *   let p = List.nth_exn words 2 |> Float.of_string in
   *   let v = List.nth_exn words 3 |> Float.of_string in
   *   send_msg w
   *     (new_order
   *        ~uuid:Uuid.(create () |> to_string)
   *        ~symbol ~p ~v ~side:Buy)
   * | "SELL" ->
   *   let symbol = List.nth_exn words 1 in
   *   let p = List.nth_exn words 2 |> Float.of_string in
   *   let v = List.nth_exn words 3 |> Float.of_string in
   *   send_msg w
   *     (new_order
   *        ~uuid:Uuid.(create () |> to_string)
   *        ~symbol ~p ~v ~side:Sell) *)

let cleanup_term w _ =
  don't_wait_for begin
    Pipe.write w logout
  end

let main cfg =
  Logs_async.debug ~src (fun m -> m "%a" Cfg.pp cfg) >>= fun () ->
  let { Cfg.key ; secret ; _ } =
    List.Assoc.find_exn ~equal:String.equal cfg "DERIBIT" in
  let ts = Ptime_clock.now () in
  let logon_fields =
    logon_fields ~cancel_on_disconnect:true ~username:key ~secret ~ts in
  Fix_async.with_connection_ez
    ~sid ~tid ~version:Version.v44 ~logon_fields uri >>= fun (r, w) ->
  Signal.(handle terminating ~f:(cleanup_term w)) ;
  Logs_async.app ~src (fun m -> m "Connected to Deribit") >>= fun () ->
  Deferred.any [
    Pipe.iter r ~f:(on_server_msg w);
    Pipe.iter Reader.(stdin |> Lazy.force |> pipe) ~f:(on_client_cmd w);
    (* heartbeat_loop w 30; *)
    Pipe.closed w
  ] >>= fun () ->
  Pipe.write w logout

let command =
  Command.async ~summary:"Deribit testnet shell" begin
    let open Command.Let_syntax in
    [%map_open
      let cfg = Cfg.param ()
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main cfg
    ]
  end

let () = Command.run command
