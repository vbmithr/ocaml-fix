open Core
open Async

open Bs_devkit
open Fix
open Fixtypes
open Deribit

let src = Logs.Src.create "fix.deribit.shell"
let uri = Uri.make ~host:"test.deribit.com" ~port:9881 ()

let send_msg =
  let count = ref 1 in
  fun w f ->
    let msg = f !count in
    incr count ;
    Pipe.write w msg

let rec on_server_msg w msg = match msg.Fix.typ with
  | Logout ->
    (* Immediately send a logout msg and exit. *)
    send_msg w logout >>= fun () ->
    Shutdown.exit 0
  | TestRequest -> begin
      match Field.find_list Field.TestReqID msg.fields with
      | None -> Deferred.unit
      | Some testreqid ->
        (* Immediately send a heartbeat with the same seqnum. *)
        send_msg w (heartbeat testreqid)
    end
  | ResendRequest -> Deferred.unit
  | _ -> Deferred.unit

let rec heartbeat_loop w period =
  Clock_ns.(after (Time_ns.Span.of_int_sec period)) >>= fun () ->
  send_msg w (heartbeat "") >>= fun () ->
  heartbeat_loop w period

(* let on_client_cmd ~secret ~passphrase w words =
 *   let words = String.split ~on:' ' @@ String.chop_suffix_exn words ~suffix:"\n" in
 *   match List.hd_exn words with
 *   | "TESTREQ" -> send_msg w (fun () -> testreq @@ Uuid.(create () |> to_string))
 *   | "BUY" ->
 *     let symbol = List.nth_exn words 1 in
 *     let p = List.nth_exn words 2 |> Float.of_string in
 *     let v = List.nth_exn words 3 |> Float.of_string in
 *     send_msg w
 *       (new_order
 *          ~uuid:Uuid.(create () |> to_string)
 *          ~symbol ~p ~v ~side:Buy)
 *   | "SELL" ->
 *     let symbol = List.nth_exn words 1 in
 *     let p = List.nth_exn words 2 |> Float.of_string in
 *     let v = List.nth_exn words 3 |> Float.of_string in
 *     send_msg w
 *       (new_order
 *          ~uuid:Uuid.(create () |> to_string)
 *          ~symbol ~p ~v ~side:Sell)
 *   | command ->
 *     info "Unsupported command: %s" command;
 *     Deferred.unit *)

let main cfg =
  Logs_async.debug ~src (fun m -> m "%a" Cfg.pp cfg) >>= fun () ->
  let { Cfg.key; secret; passphrase } =
    List.Assoc.find_exn ~equal:String.equal cfg "DERIBIT" in
  Fix_async.with_connection ~version:Version.v44 uri >>= fun (r, w) ->
  Signal.(handle terminating ~f:(fun _ -> don't_wait_for @@ send_msg w logout));
  Logs_async.app ~src (fun m -> m "Connected to Deribit") >>= fun () ->
  let ts = Ptime_clock.now () in
  send_msg w (logon ~username:key ~secret ~ts) >>= fun () ->
  Deferred.any [
    Pipe.iter r ~f:(on_server_msg w);
    (* Pipe.iter Reader.(stdin |> Lazy.force |> pipe) ~f:(on_client_cmd ~secret ~passphrase w); *)
    heartbeat_loop w 30;
    Pipe.closed w
  ] >>= fun () ->
  send_msg w logout

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
