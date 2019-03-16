open Core
open Async

open Fix
open Fixtypes
open Fix_coinbasepro

let src = Logs.Src.create "fix.coinbase.shell"
(* let uri = Uri.make ~host:"127.0.0.1" ~port:4197 () *)

let hb msg =
  Fix.create ~fields:[Field.TestReqID.create msg] MsgType.Heartbeat

let on_server_msg _w msg = match msg.Fix.typ with
  | _ -> Deferred.unit

let on_client_cmd w words =
  let words = String.split ~on:' ' @@ String.chop_suffix_exn words ~suffix:"\n" in
  match words with
  | "testreq" :: _ -> Pipe.write w (testreq ~testreqid:"a")
  | "orders" :: _ -> Pipe.write w (order_status_request ())
  | "order" :: orderID :: _ -> Pipe.write w (order_status_request ~orderID ())
  | "buy" :: symbol :: qty :: [] ->
    let clOrdID = Uuidm.create `V4 in
    let qty = float_of_string qty in
    Pipe.write w
      (new_order_market ~side:Buy ~qty ~symbol clOrdID)
  | "sell" :: symbol :: qty :: [] ->
    let clOrdID = Uuidm.create `V4 in
    let qty = float_of_string qty in
    Pipe.write w
      (new_order_market ~side:Sell ~qty ~symbol clOrdID)
  | "buy" :: symbol :: qty :: price :: _ ->
    let clOrdID = Uuidm.create `V4 in
    let price = float_of_string price in
    let qty = float_of_string qty in
    let timeInForce = Fixtypes.TimeInForce.GoodTillCancel in
    Pipe.write w
      (new_order_limit ~side:Buy ~price ~qty ~timeInForce ~symbol clOrdID)
  | "sell" :: symbol :: qty :: price :: _ ->
    let clOrdID = Uuidm.create `V4 in
    let price = float_of_string price in
    let qty = float_of_string qty in
    let timeInForce = Fixtypes.TimeInForce.GoodTillCancel in
    Pipe.write w
      (new_order_limit ~side:Sell ~price ~qty ~timeInForce ~symbol clOrdID)
  | "cancel" :: srvOrdID :: _ -> begin
    match Uuidm.of_string srvOrdID with
    | None -> Logs_async.err ~src (fun m -> m "wrong srvOrdID: must be an UUID")
    | Some srvOrdID -> Pipe.write w (cancel_order ~srvOrdID)
  end
  | _ ->
    Logs_async.app ~src (fun m -> m "Unsupported command")

let main sandbox cfg =
  let open Bs_devkit in
  let url = if sandbox then sandbox_url else url in
  let { Cfg.key ; secret ; passphrase ; _ } =
    List.Assoc.find_exn ~equal:String.equal cfg "CBPRO" in
  let secret = Base64.decode_exn secret in
  let logon_ts = Ptime_clock.now () in
  let logon_fields =
    logon_fields ~cancel_on_disconnect:`Session ~key ~secret ~passphrase ~logon_ts in
  Fix_async.connect_ez
    ~logon_ts
    ~heartbeat:(Time_ns.Span.of_int_sec 30)
    ~sid:key ~tid ~version:Version.v42 ~logon_fields url >>= fun (r, w) ->
  Signal.(handle terminating ~f:(fun _ -> Pipe.close w)) ;
  Logs_async.app ~src (fun m -> m "Connected to Coinbase") >>= fun () ->
  Deferred.any [
    Pipe.iter r ~f:(on_server_msg w);
    Pipe.iter Reader.(stdin |> Lazy.force |> pipe) ~f:(on_client_cmd w);
    Pipe.closed w
  ]

let command =
  Command.async ~summary:"Coinbase sandbox shell" begin
    let open Command.Let_syntax in
    [%map_open
      let cfg = Bs_devkit.Cfg.param ()
      and sandbox = flag "sandbox" no_arg ~doc:" Use sandbox"
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main sandbox cfg
    ]
  end

let () = Command.run command
