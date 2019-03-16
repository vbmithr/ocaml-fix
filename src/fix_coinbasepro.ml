open Fix
open Field

module CancelOnDisconnect = struct
  module T = struct
    type t =
      | All
      | Session [@@deriving sexp]

    let parse = function
      | "Y" -> Some All
      | "S" -> Some Session
      | _ -> None

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
    [@@deriving sexp]

    let parse = function
      | "D" -> Some DecrementAndCancel
      | "O" -> Some CancelRestingOrder
      | "N" -> Some CancelIncomingOrder
      | "B" -> Some CancelBothOrders
      | _ -> None

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
    type t = CancelOnDisconnect.t [@@deriving sexp]
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
    type t = SelfTradePrevention.t [@@deriving sexp]
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

let cancel_order ~srvOrdID =
  let fields = [
    Field.OrderID.create (Uuidm.to_string srvOrdID) ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.OrderCancelRequest
