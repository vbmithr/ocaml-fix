open Core
open Async

open Fix
open Fixtypes
open Fix_coinbasepro

let src = Logs.Src.create "fix.coinbase.shell"
let uri = Uri.make ~host:"127.0.0.1" ~port:4199 ()

let hb msg =
  Fix.create ~fields:[Field.TestReqID.create msg] MsgType.Heartbeat

let on_server_msg _w msg = match msg.Fix.typ with
  | _ -> Deferred.unit

let on_client_cmd username w words =
  let words = String.split ~on:' ' @@ String.chop_suffix_exn words ~suffix:"\n" in
  match words with
  | "testreq" :: _ ->
    let fields = [Field.TestReqID.create "a"] in
    Pipe.write w (Fix.create ~fields MsgType.TestRequest)
  | "seclist" :: _ ->
    let fields = [
      Field.SecurityReqID.create "a" ;
      Field.SecurityListRequestType.create Symbol ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.SecurityListRequest)
  | "snapshot" :: symbol :: _ ->
    let fields = [
      Field.Symbol.create symbol ;
      Field.MDReqID.create "a" ;
      Field.SubscriptionRequestType.create Snapshot ;
      Field.MarketDepth.create 0 ;
    ] in
    let groups =
      Field.NoMDEntryTypes.create 3, [
        [ Field.MDEntryType.create Bid ] ;
        [ Field.MDEntryType.create Offer ] ;
        [ Field.MDEntryType.create Trade ] ;
      ] in
    Pipe.write w (Fix.create ~groups ~fields MsgType.MarketDataRequest)
  | "stream" :: symbol :: _ ->
    let fields = [
      Field.Symbol.create symbol ;
      Field.MDReqID.create "a" ;
      Field.SubscriptionRequestType.create Subscribe ;
      Field.MDUpdateType.create Incremental ;
      Field.MarketDepth.create 0 ;
    ] in
    let groups =
      Field.NoMDEntryTypes.create 3, [
        [ Field.MDEntryType.create Bid ] ;
        [ Field.MDEntryType.create Offer ] ;
        [ Field.MDEntryType.create Trade ] ;
      ] in
    Pipe.write w (Fix.create ~groups ~fields MsgType.MarketDataRequest)
  | "orders" :: _ ->
    let fields = [
      Field.OrderID.create "*" ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.OrderStatusRequest)
  | "order" :: id :: _ ->
    let fields = [
      Field.OrderID.create id ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.OrderStatusRequest)
  | "positions" :: _ ->
    let fields = [
      Field.PosReqID.create Uuid.(create () |> to_string) ;
      Field.PosReqType.create Positions ;
      Field.SubscriptionRequestType.create Snapshot ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.RequestForPositions)
  | "info" :: _ ->
    let fields = [
      Field.UserRequestID.create "a" ;
      Field.UserRequestType.create RequestStatus ;
      Field.Username.create username ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.UserRequest)
  | "buy" :: symbol :: qty :: [] ->
    let fields = [
      Field.HandlInst.create Private ;
      Field.ClOrdID.create Uuid.(create () |> to_string) ;
      Field.Side.create Buy ;
      Field.OrderQty.create (float_of_string qty) ;
      Field.OrdType.create Market ;
      Field.Symbol.create symbol ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.NewOrderSingle)
  | "sell" :: symbol :: qty :: [] ->
    let fields = [
      Field.HandlInst.create Private ;
      Field.ClOrdID.create Uuid.(create () |> to_string) ;
      Field.Side.create Sell ;
      Field.OrderQty.create (float_of_string qty) ;
      Field.OrdType.create Market ;
      Field.Symbol.create symbol ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.NewOrderSingle)
  | "buy" :: symbol :: qty :: price :: _ ->
    let fields = [
      Field.HandlInst.create Private ;
      Field.ClOrdID.create Uuid.(create () |> to_string) ;
      Field.Side.create Buy ;
      Field.OrderQty.create (float_of_string qty) ;
      Field.Price.create (float_of_string price) ;
      Field.OrdType.create Limit ;
      Field.TimeInForce.create GoodTillCancel ;
      Field.Symbol.create symbol ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.NewOrderSingle)
  | "sell" :: symbol :: qty :: price :: _ ->
    let fields = [
      Field.HandlInst.create Private ;
      Field.ClOrdID.create Uuid.(create () |> to_string) ;
      Field.Side.create Sell ;
      Field.OrderQty.create (float_of_string qty) ;
      Field.Price.create (float_of_string price) ;
      Field.OrdType.create Limit ;
      Field.TimeInForce.create GoodTillCancel ;
      Field.Symbol.create symbol ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.NewOrderSingle)
  | "cancel" :: srvOrdID :: _ ->
    let fields = [
      Field.OrderID.create srvOrdID ;
    ] in
    Pipe.write w (Fix.create ~fields MsgType.OrderCancelRequest)
  | _ ->
    Logs_async.app ~src (fun m -> m "Unsupported command")

let main cfg =
  let open Bs_devkit in
  Logs_async.debug ~src (fun m -> m "%a" Cfg.pp cfg) >>= fun () ->
  let { Cfg.key ; secret ; passphrase ; _ } =
    List.Assoc.find_exn ~equal:String.equal cfg "COINBASE-SANDBOX2" in
  let secret = Base64.decode_exn secret in
  let logon_ts = Ptime_clock.now () in
  let logon_fields =
    logon_fields ~cancel_on_disconnect:`Session ~key ~secret ~passphrase ~logon_ts in
  Fix_async.with_connection_ez
    ~logon_ts
    ~heartbeat:(Time_ns.Span.of_int_sec 30)
    ~sid:key ~tid ~version:Version.v42 ~logon_fields uri >>= fun (closed, r, w) ->
  Signal.(handle terminating ~f:(fun _ -> Pipe.close w)) ;
  Logs_async.app ~src (fun m -> m "Connected to Coinbase") >>= fun () ->
  Deferred.any [
    Pipe.iter r ~f:(on_server_msg w);
    Pipe.iter Reader.(stdin |> Lazy.force |> pipe) ~f:(on_client_cmd key w);
    closed
  ]

let command =
  Command.async ~summary:"Coinbase sandbox shell" begin
    let open Command.Let_syntax in
    [%map_open
      let cfg = Bs_devkit.Cfg.param ()
      and () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main cfg
    ]
  end

let () = Command.run command
