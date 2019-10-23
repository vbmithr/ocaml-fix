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

    let to_string = function
      | DecrementAndCancel -> "D"
      | CancelRestingOrder -> "O"
      | CancelIncomingOrder -> "N"
      | CancelBothOrders -> "B"
  end
  include T
  include Fixtypes.Make(T)
end

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

type _ typ += BatchID : string typ
module BatchID = Make(struct
    type t = string [@@deriving sexp,yojson]
    let t = BatchID
    let pp = Format.pp_print_string
    let parse s = Ok s
    let tag = 8014
    let name = "BatchID"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | BatchID, BatchID -> Some Eq
      | _ -> None
  end)
let () = register_field (module BatchID)

let tid = "Coinbase"
let version = Version.v42
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

let order_status_request orderID =
  let fields = [
    Field.OrderID.create (Uuidm.to_string orderID) ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.OrderStatusRequest

let supported_timeInForces = [
  Fixtypes.TimeInForce.GoodTillCancel ;
  ImmediateOrCancel ;
  FillOrKill ;
  PostOnly ;
]

let supported_ordTypes = [
  Fixtypes.OrdType.Market ;
  Limit ;
  StopLimit ;
]

let new_order_fields
    ?selfTradePrevention
    ?(timeInForce=Fixtypes.TimeInForce.GoodTillCancel)
    ?(ordType=Fixtypes.OrdType.Market)
    ?price
    ?stopPx
    ~side
    ~qty
    ~symbol
    clOrdID =
  if not List.(mem timeInForce supported_timeInForces &&
               mem ordType supported_ordTypes) then
    invalid_arg "unsupported tif or ordType" ;
  List.filter_map (fun a -> a) [
    Option.some @@ Field.HandlInst.create Private ;
    Option.some @@ Field.ClOrdID.create (Uuidm.to_string clOrdID) ;
    Option.some @@ Field.Symbol.create symbol ;
    Option.some @@ Field.Side.create side ;
    Option.some @@ Field.OrderQty.create qty ;
    Option.map Field.Price.create price ;
    Option.map Field.StopPx.create stopPx ;
    Option.some @@ Field.OrdType.create ordType ;
    Option.some @@ Field.TimeInForce.create timeInForce ;
    Option.map STP.create selfTradePrevention ;
  ]

let new_order
    ?selfTradePrevention
    ?timeInForce
    ?ordType
    ?price
    ?stopPx
    ~side ~qty ~symbol clOrdID =
  let fields =
    new_order_fields
      ?selfTradePrevention ?timeInForce ?ordType ?price ?stopPx
      ~side ~qty ~symbol clOrdID in
  Fix.create ~fields Fixtypes.MsgType.NewOrderSingle

let new_orders_limit batchID fieldss =
  Fix.create
    ~fields:[ BatchID.create (Uuidm.to_string batchID) ]
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
    ~fields:[ BatchID.create (Uuidm.to_string batchID) ]
    ~groups:(NoOrders.create (List.length orders), orders)
    Fixtypes.MsgType.OrderCancelBatchRequest

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
  text: string option ;
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
        (Field.Set.find_typ_bind BatchID t.fields ~f:Uuidm.of_string) in
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
