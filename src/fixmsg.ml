open Sexplib.Std
open Fixtypes
open Fix
open Field

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

let parse t =
  match t.typ with
  | Logon -> Logon
  | Logout -> Logout
  | TestRequest -> TestRequest (Field.Set.find_typ_exn TestReqID t.fields)
  | Heartbeat -> Heartbeat (Field.Set.find_typ TestReqID t.fields)
  | ExecutionReport ->
    let clOrdID      = Field.Set.find_typ_bind ClOrdID t.fields ~f:Uuidm.of_string in
    let orderID      = Field.Set.find_typ_bind OrderID t.fields ~f:Uuidm.of_string in
    let tradeID      = Field.Set.find_typ_bind TradeID t.fields ~f:Uuidm.of_string in
    let symbol       = Field.Set.find_typ_exn Symbol t.fields in
    let side         = Field.Set.find_typ_exn Side t.fields in
    let lastQty      = Field.Set.find_typ LastQty t.fields in
    let price        = Field.Set.find_typ Price t.fields in
    let orderQty     = Field.Set.find_typ_exn OrderQty t.fields in
    let transactTime = Field.Set.find_typ TransactTime t.fields in
    let ordStatus    = Field.Set.find_typ_exn OrdStatus t.fields in
    let taker        = Field.Set.find_typ AggressorIndicator t.fields in
    let ordRejReason = Field.Set.find_typ OrdRejReason t.fields in
    let execType     = Field.Set.find_typ_exn ExecType t.fields in
    let text         = Field.Set.find_typ Text t.fields in
    ExecutionReport { clOrdID; orderID; tradeID; symbol; execType; side; lastQty; price;
                      orderQty; transactTime; ordStatus; taker; ordRejReason; text }
  | NewOrderBatchReject ->
    let batchID =
      Option.get
        (Field.Set.find_typ_bind Fix_coinbasepro.BatchID
           t.fields ~f:Uuidm.of_string) in
    let text = Field.Set.find_typ Text t.fields in
    NewOrderBatchReject { batchID; text }
  | OrderCancelReject ->
    let clOrdID = Option.get (Field.Set.find_typ_bind ClOrdID t.fields ~f:Uuidm.of_string) in
    let orderID = Option.get (Field.Set.find_typ_bind OrderID t.fields ~f:Uuidm.of_string) in
    let origClOrdID = Field.Set.find_typ_bind OrigClOrdID t.fields ~f:Uuidm.of_string in
    let ordStatus = Field.Set.find_typ OrdStatus t.fields in
    let cxlRejReason = Field.Set.find_typ_exn CxlRejReason t.fields in
    let cxlRejResponseTo = Field.Set.find_typ_exn CxlRejResponseTo t.fields in
    OrderCancelReject { clOrdID; orderID; origClOrdID;
                        ordStatus; cxlRejReason; cxlRejResponseTo }
  | _ -> Format.kasprintf invalid_arg "received %a" Fixtypes.MsgType.pp t.typ

let new_order_fields
    ?selfTradePrevention
    ?(timeInForce=Fixtypes.TimeInForce.GoodTillCancel)
    ?(ordType=Fixtypes.OrdType.Market)
    ?execInst
    ?price
    ?stopPx
    ~side
    ~qty
    ~symbol
    clOrdID =
  List.filter_map (fun a -> a) [
    Option.some @@ Field.HandlInst.create Private ;
    Option.some @@ Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    Option.some @@ Field.Symbol.create symbol ;
    Option.some @@ Field.Side.create side ;
    Option.some @@ Field.OrderQty.create qty ;
    Option.map Field.Price.create price ;
    Option.map Field.StopPx.create stopPx ;
    Option.map Field.ExecInst.create execInst ;
    Option.some @@ Field.OrdType.create ordType ;
    Option.some @@ Field.TimeInForce.create timeInForce ;
    Option.map Fix_coinbasepro.STP.create selfTradePrevention ;
  ]

let new_order
    ?selfTradePrevention
    ?timeInForce
    ?ordType
    ?execInst
    ?price
    ?stopPx
    ~side ~qty ~symbol clOrdID =
  let fields =
    new_order_fields ?execInst
      ?selfTradePrevention ?timeInForce ?ordType ?price ?stopPx
      ~side ~qty ~symbol clOrdID in
  Fix.create ~fields Fixtypes.MsgType.NewOrderSingle

let new_orders_limit batchID fieldss =
  Fix.create
    ~fields:[ Fix_coinbasepro.BatchID.create (Uuidm.to_string batchID) ]
    ~groups:(NoOrders.create (List.length fieldss), fieldss)
    Fixtypes.MsgType.NewOrderBatch

let cancel_order ~orderID ~clOrdID =
  let fields = [
    Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    match orderID with
    | `ClOrdID id -> Field.OrigClOrdID.create (Uuidm.to_string id) ;
    | `OrderID id -> Field.OrderID.create (Uuidm.to_string id) ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.OrderCancelRequest

let cancel_orders batchID ~symbol orders =
  let orders =
    List.map begin fun (clOrdID, sv) -> List.filter_map (fun a -> a) [
      Some (Field.OrigClOrdID.create (Uuidm.to_string clOrdID)) ;
      Some (Field.Symbol.create symbol) ;
      Option.map (fun sv -> Field.OrderID.create (Uuidm.to_string sv)) sv ;
    ]
    end orders in
  Fix.create
    ~fields:[ Fix_coinbasepro.BatchID.create (Uuidm.to_string batchID) ]
    ~groups:(NoOrders.create (List.length orders), orders)
    Fixtypes.MsgType.OrderCancelBatchRequest

