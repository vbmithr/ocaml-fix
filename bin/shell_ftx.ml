open Core
open Async

open Fix
open Fixtypes
open Fix_ftx

let src = Logs.Src.create "fix.ftx.shell"

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
  | "buy" :: symbol :: qty :: price :: _ ->
    let clOrdID = Uuidm.create `V4 in
    let price = float_of_string price in
    let qty = float_of_string qty in
    let timeInForce = Fixtypes.TimeInForce.GoodTillCancel in
    Pipe.write w
      (new_order ~side:Buy ~price ~qty ~timeInForce ~symbol clOrdID)
  | "sell" :: symbol :: qty :: price :: _ ->
    let clOrdID = Uuidm.create `V4 in
    let price = float_of_string price in
    let qty = float_of_string qty in
    let timeInForce = Fixtypes.TimeInForce.GoodTillCancel in
    Pipe.write w
      (new_order ~side:Sell ~price ~qty ~timeInForce ~symbol clOrdID)
  | "cancel" :: srvOrdID :: _ ->
    Pipe.write w (cancel_order (`OrderID (Int64.of_string srvOrdID)))
  | "cancelClient" :: origClOrdID :: _ -> begin
    match Uuidm.of_string origClOrdID with
      | None -> Logs_async.err ~src (fun m -> m "wrong origClOrdID: must be an UUID")
      | Some orderID ->  Pipe.write w (cancel_order (`ClOrdID orderID))
  end
  | _ ->
    Logs_async.app ~src (fun m -> m "Unsupported command")

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_FTX") with
  | [key; secret] ->
    key, secret
  | _ -> assert false

let main () =
  let logon_ts = Ptime_clock.now () in
  let logon_fields =
    logon_fields ~cancel_on_disconnect:`Session ~key ~secret ~logon_ts in
  Fix_async.with_connection
    ~logon_ts
    ~heartbeat:(Time_ns.Span.of_int_sec 30)
    ~sid:key ~tid ~version:Version.v42 ~logon_fields url
    ~f:begin fun r w ->
      Signal.(handle terminating ~f:(fun _ -> Pipe.close w)) ;
      Logs_async.app ~src (fun m -> m "Connected to FTX") >>= fun () ->
      Deferred.any [
        Deferred.all_unit [Pipe.closed w; Pipe.closed r] ;
        Pipe.iter r ~f:(on_server_msg w);
        Pipe.iter Reader.(stdin |> Lazy.force |> pipe) ~f:(on_client_cmd w);
      ] >>= fun () ->
      Deferred.Or_error.ok_unit
    end

let command =
  Command.async_or_error ~summary:"FTX shell" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ]
  end

let () = Command.run command
