open Rresult
open Sexplib.Std
open Fix
open Fixtypes
open Field

module Uuidm = struct
  include Uuidm

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_string sexp_str with
    | None -> invalid_arg "Uuidm.t_of_sexp"
    | Some u -> u

  let sexp_of_t t =
    sexp_of_string (to_string t)

  let of_yojson = function
    | `String s -> begin
      match of_string s with
      | None -> Error "not an uuid"
      | Some u -> Ok u
    end
    | #Yojson.Safe.t -> Error "not a json string"

  let to_yojson t = `String (to_string t)
end

module CancelOnDisconnect = struct
  module T = struct
    type t =
      | All
      | Session
    [@@deriving sexp,yojson]

    let parse = function
      | "Y" -> Ok All
      | "S" -> Ok Session
      | _ -> R.error_msg "invalid argument"

    let print = function
      | All -> "Y"
      | Session -> "S"
  end
  include T
  include Fixtypes.Make(T)
end

module SelfTradePrevention = struct
  module T = struct
    type t =
      | DecrementAndCancel
      | CancelRestingOrder
      | CancelIncomingOrder
      | CancelBothOrders
    [@@deriving sexp,yojson]

    let parse = function
      | "D" -> Ok DecrementAndCancel
      | "O" -> Ok CancelRestingOrder
      | "N" -> Ok CancelIncomingOrder
      | "B" -> Ok CancelBothOrders
      | _ -> R.error_msg "invalid argument"

    let print = function
      | DecrementAndCancel -> "D"
      | CancelRestingOrder -> "O"
      | CancelIncomingOrder -> "N"
      | CancelBothOrders -> "B"
  end
  include T
  include Fixtypes.Make(T)
end

type _ typ += CancelOnDisconnect : CancelOnDisconnect.t typ
module CancelOrdersOnDisconnect = Make(struct
    type t = CancelOnDisconnect.t [@@deriving sexp,yojson]
    let t = CancelOnDisconnect
    let pp = CancelOnDisconnect.pp
    let parse = CancelOnDisconnect.parse
    let tag = 8013
    let name = "CancelOnDisconnect"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | CancelOnDisconnect, CancelOnDisconnect -> Some Eq
      | _ -> None
  end)
let () = register_field (module CancelOrdersOnDisconnect)

type _ typ += SelfTradePrevention : SelfTradePrevention.t typ
module STP = Make(struct
    type t = SelfTradePrevention.t [@@deriving sexp,yojson]
    let t = SelfTradePrevention
    let pp = SelfTradePrevention.pp
    let parse = SelfTradePrevention.parse
    let tag = 7928
    let name = "SelfTradePrevention"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SelfTradePrevention, SelfTradePrevention -> Some Eq
      | _ -> None
  end)
let () = register_field (module STP)

let tid = "Coinbase"
let url = Uri.make ~scheme:"https"
    ~host:"fix.pro.coinbase.com" ~port:4198 ()
let sandbox_url = Uri.make ~scheme:"https"
    ~host:"fix-public.sandbox.pro.coinbase.com" ~port:4198 ()

let logon_fields
    ?cancel_on_disconnect
    ~key
    ~secret
    ~passphrase
    ~logon_ts =
  let prehash = String.concat "\x01" [
      Format.asprintf "%a" Fixtypes.UTCTimestamp.pp logon_ts ;
      Format.asprintf "%a" MsgType.pp Logon ;
      "1" ; key ; tid ; passphrase ;
    ] in
  let rawdata =
    Base64.encode_exn
      Digestif.SHA256.(hmac_string ~key:secret prehash |> to_raw_string) in
  List.rev_append [
    EncryptMethod.create Other ;
    RawData.create rawdata ;
    Password.create passphrase ;
  ]
    begin
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

let new_order_market
    ?(selfTradePrevention=SelfTradePrevention.DecrementAndCancel)
    ~side ~qty ~symbol clOrdID =
  let fields = [
    Field.HandlInst.create Private ;
    Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    Field.Side.create side ;
    Field.OrderQty.create qty ;
    Field.OrdType.create Market ;
    Field.Symbol.create symbol ;
    STP.create selfTradePrevention ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.NewOrderSingle

let check_time_in_force = function
  | Fixtypes.TimeInForce.GoodTillCancel
  | ImmediateOrCancel
  | FillOrKill
  | PostOnly -> ()
  | _ -> invalid_arg "timeInForce not supported by Coinbasepro"

let new_order_limit
    ?(selfTradePrevention=SelfTradePrevention.DecrementAndCancel)
    ~side ~price ~qty ~timeInForce ~symbol clOrdID =
  check_time_in_force timeInForce ;
  let fields = [
    Field.HandlInst.create Private ;
    Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    Field.Side.create side ;
    Field.OrderQty.create qty ;
    Field.Price.create price ;
    Field.OrdType.create Limit ;
    Field.TimeInForce.create timeInForce ;
    Field.Symbol.create symbol ;
    STP.create selfTradePrevention ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.NewOrderSingle

let new_order_stop
    ?(selfTradePrevention=SelfTradePrevention.DecrementAndCancel)
    ~side ~stopPx ~qty ~timeInForce ~symbol clOrdID =
  check_time_in_force timeInForce ;
  let fields = [
    Field.HandlInst.create Private ;
    Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    Field.Side.create side ;
    Field.OrderQty.create qty ;
    Field.StopPx.create stopPx ;
    Field.OrdType.create Stop ;
    Field.TimeInForce.create timeInForce ;
    Field.Symbol.create symbol ;
    STP.create selfTradePrevention ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.NewOrderSingle

let new_order_stop_limit
    ?(selfTradePrevention=SelfTradePrevention.DecrementAndCancel)
    ~side ~price ~stopPx ~qty ~timeInForce ~symbol clOrdID =
  check_time_in_force timeInForce ;
  let fields = [
    Field.HandlInst.create Private ;
    Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    Field.Side.create side ;
    Field.OrderQty.create qty ;
    Field.Price.create price ;
    Field.StopPx.create stopPx ;
    Field.OrdType.create StopLimit ;
    Field.TimeInForce.create timeInForce ;
    Field.Symbol.create symbol ;
    STP.create selfTradePrevention ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.NewOrderSingle

let cancel_order ~orderID ~clOrdID =
  let fields = [
    Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    match orderID with
    | `ClOrdID id -> Field.OrigClOrdID.create (Uuidm.to_string id) ;
    | `OrderID id -> Field.OrderID.create (Uuidm.to_string id) ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.OrderCancelRequest

type execution_report = {
  clOrdID : Uuidm.t option ;
  orderID : Uuidm.t option ;
  symbol : string ;
  side : Fixtypes.Side.t ;
  lastQty : float option ;
  price : float ;
  orderQty : float ;
  cashOrderQty : float option ;
  transactTime : Ptime.t ;
  ordStatus : Fixtypes.OrdStatus.t ;
  ordRejReason : Fixtypes.OrdRejReason.t option ;
  tradeID : Uuidm.t ;
  taker : bool ;
} [@@deriving sexp,yojson]

let parse_execution_report t =
  match t.typ with
  | ExecutionReport ->
    let clOrdID =
      Field.find_set_bind ClOrdID t.fields ~f:Uuidm.of_string in
    let orderID =
      Field.find_set_bind OrderID t.fields ~f:Uuidm.of_string in
    let tradeID =
      Field.find_set_bind TradeID t.fields ~f:Uuidm.of_string in
    let symbol = Field.find_set Symbol t.fields in
    let side = Field.find_set Side t.fields in
    let lastQty = Field.find_set LastQty t.fields in
    let price = Field.find_set Price t.fields in
    let orderQty = Field.find_set OrderQty t.fields in
    let cashOrderQty = Field.find_set CashOrderQty t.fields in
    let transactTime = Field.find_set TransactTime t.fields in
    let ordStatus = Field.find_set OrdStatus t.fields in
    let taker = Field.find_set AggressorIndicator t.fields in
    let ordRejReason = Field.find_set OrdRejReason t.fields in
    begin
      match symbol, side, price, orderQty,
            transactTime, ordStatus, tradeID, taker with
      | Some symbol, Some side, Some price, Some orderQty,
        Some transactTime, Some ordStatus, Some tradeID, Some taker ->
        { clOrdID ; orderID ; symbol ; side ;
          lastQty ; price ; orderQty ; cashOrderQty ; transactTime ;
          ordStatus ; tradeID ; taker ; ordRejReason }
      | _ -> invalid_arg "parse_execution_report"
    end
  | _ -> invalid_arg "not an ExecutionReport"

type order_cancel_reject = {
  clOrdID : Uuidm.t option ;
  orderID : Uuidm.t option ;
  origClOrderID : Uuidm.t option ;
  ordStatus : Fixtypes.OrdStatus.t option ;
  cxlRejReason : Fixtypes.CxlRejReason.t option ;
  cxlRejResponseTo : Fixtypes.CxlRejResponseTo.t ;
} [@@deriving sexp,yojson]

let parse_order_cancel_reject t =
  match t.typ with
  | OrderCancelReject ->
    let clOrdID =
      Field.find_set_bind ClOrdID t.fields ~f:Uuidm.of_string in
    let orderID =
      Field.find_set_bind OrderID t.fields ~f:Uuidm.of_string in
    let origClOrderID =
      Field.find_set_bind OrigClOrdID t.fields ~f:Uuidm.of_string in
    let ordStatus = Field.find_set OrdStatus t.fields in
    let cxlRejReason = Field.find_set CxlRejReason t.fields in
    let cxlRejResponseTo = Field.find_set CxlRejResponseTo t.fields in
    begin
      match cxlRejResponseTo with
      | None -> invalid_arg "parse_order_cancel_reject"
      | Some cxlRejResponseTo ->
        { clOrdID ; orderID ; origClOrderID ; ordStatus ;
          cxlRejReason ; cxlRejResponseTo }
    end
  | _ -> invalid_arg "not an OrderCancelReject"

