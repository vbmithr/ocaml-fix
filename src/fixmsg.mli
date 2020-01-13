open Fixtypes
open Fix

type executionReport = {
  clOrdID : Uuidm.t option ;
  orderID : Uuidm.t option ;
  symbol : string ;
  execType : Fixtypes.ExecType.t ;
  side : Fixtypes.Side.t ;
  lastQty : float option ;
  price : float option ;
  orderQty : float ;
  transactTime : Ptime.t option ;
  ordStatus : Fixtypes.OrdStatus.t ;
  ordRejReason : Fixtypes.OrdRejReason.t option ;
  tradeID : Uuidm.t option ;
  taker : bool option ;
  text : string option ;
} [@@deriving sexp,yojson]

val new_order_fields :
  ?selfTradePrevention:Fix_coinbasepro.SelfTradePrevention.t ->
  ?timeInForce:TimeInForce.t ->
  ?ordType:OrdType.t ->
  ?execInst:ExecInst.t ->
  ?price:float ->
  ?stopPx:float ->
  side:Side.t -> qty:float -> symbol:string -> Uuidm.t -> Field.t list

val new_order :
  ?selfTradePrevention:Fix_coinbasepro.SelfTradePrevention.t ->
  ?timeInForce:TimeInForce.t ->
  ?ordType:OrdType.t ->
  ?execInst:ExecInst.t ->
  ?price:float ->
  ?stopPx:float -> side:Side.t -> qty:float ->
  symbol:string -> Uuidm.t -> Fix.t

val new_orders_limit : Uuidm.t -> Field.t list list -> Fix.t

val cancel_order :
  orderID:[`ClOrdID of Uuidm.t | `OrderID of Uuidm.t] ->
  clOrdID:Uuidm.t -> Fix.t

val cancel_orders :
  Uuidm.t -> symbol:string -> (Uuidm.t * Uuidm.t option) list -> Fix.t

type t =
  | Logon
  | Logout
  | TestRequest of string
  | Heartbeat of string option
  | ExecutionReport of executionReport
  | NewOrderBatchReject of {
      batchID: Uuidm.t; text: string option
    }
  | OrderCancelReject of {
      clOrdID: Uuidm.t;
      orderID: Uuidm.t;
      origClOrdID: Uuidm.t option;
      ordStatus: Fixtypes.OrdStatus.t option;
      cxlRejReason: Fixtypes.CxlRejReason.t ;
      cxlRejResponseTo : Fixtypes.CxlRejResponseTo.t ;
    }
  | OrderCancelBatchReject of {
      batchID: Uuidm.t; text: string option
    }
  | Reject
[@@deriving sexp,yojson]

val parse : Fix.t -> t
