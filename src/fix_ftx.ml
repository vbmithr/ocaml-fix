open Fix
open Fixtypes
open Field

let tid = "FTX"
let url = Uri.make ~scheme:"https" ~host:"fix.ftx.com" ~port:4363 ()

let logon_fields
    ?cancel_on_disconnect
    ~key
    ~secret
    ~logon_ts =
  let prehash = String.concat "\x01" [
      Format.asprintf "%a" UTCTimestamp.pp logon_ts ;
      Format.asprintf "%a" MsgType.pp Logon ;
      "1" ; key ; tid
    ] in
  let `Hex rawdata =
    Hex.of_string
      Digestif.SHA256.(hmac_string ~key:secret prehash |> to_raw_string) in
  List.rev_append [
    EncryptMethod.create Other ;
    RawData.create rawdata
  ] begin
    match cancel_on_disconnect with
    | None -> []
    | Some `All -> [CancelOrdersOnDisconnect.create All] ;
    | Some `Session -> [CancelOrdersOnDisconnect.create Session] ;
  end

let testreq ~testreqid =
  let fields = [Field.TestReqID.create testreqid] in
  Fix.create ~fields Fixtypes.MsgType.TestRequest

let order_status_request ?(orderID = "*") () =
  let fields = [
    Field.OrderID.create orderID ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.OrderStatusRequest

let check_time_in_force = function
  | Fixtypes.TimeInForce.GoodTillCancel -> ()
  | _ -> invalid_arg "timeInForce not supported by FTX"

let new_order ?(reduceOnly=false) ~side ~price ~qty ~timeInForce ~symbol clOrdID =
  check_time_in_force timeInForce ;
  let fields = [
    Field.HandlInst.create Private ;
    Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    Field.Side.create side ;
    Field.OrderQty.create qty ;
    Field.Price.create price ;
    Field.OrdType.create Limit ;
    Field.Symbol.create symbol ;
    Field.TimeInForce.create timeInForce ;
  ] in
  let fields = if reduceOnly then
      Field.ExecInst.create DoNotIncrease :: fields else fields in
  Fix.create ~fields Fixtypes.MsgType.NewOrderSingle

let cancel_order order =
  let fields = [
    match order with
    | `ClOrdID id -> Field.OrigClOrdID.create (Uuidm.to_string id) ;
    | `OrderID id -> Field.OrderID.create (Int64.to_string id) ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.OrderCancelRequest
