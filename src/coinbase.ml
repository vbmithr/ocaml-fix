open Fix
module SHA256 = Rakia.SHA256.Bytes

let init ?seq apikey =
  Factory.create ?seq ~targetcompid:"Coinbase" ~sendercompid:apikey ()

let logon ?(heartbeat=30) ~secret ~passphrase t =
  let fields = Tag.[
    EncryptMethod, "0";
    HeartBtInt, string_of_int heartbeat;
    Password, passphrase;
  ]
  in
  let msg = create t Logon fields in
  (* Now adding signature *)
  (* See https://docs.exchange.coinbase.com/?javascript#logon *)
  let prehash_tags = Tag.[
      SendingTime;
      MsgType;
      MsgSeqNum;
      SenderCompID;
      TargetCompID;
      Password
    ]
  in
  let fields_with_msgtype = Tag.Map.add MsgType (MsgType.to_string msg.typ) msg.fields in
  let prehash_tags = List.map (fun tag -> Tag.Map.find tag fields_with_msgtype) prehash_tags in
  let prehash_str = String.concat "\001" prehash_tags in
  let secret = B64.decode secret in
  let signature = B64.encode (SHA256.hmac ~key:secret prehash_str) in
  add_field msg RawData signature

let logout t =
  create t Logout []

let heartbeat ?testreqid t =
  create t Heartbeat (match testreqid with None -> [] | Some id -> [TestReqID, id])

(* let testreq id t = *)
(*   let seq = t.create TestRequest [TestReqID, id] *)

let coinbase_of_symbol = function
  | "XBTUSD" -> "BTC-USD"
  | "XBTEUR" -> "BTC-EUR"
  | _ -> invalid_arg "coinbase_of_symbol"

let new_order ?(order_type="2") ?(tif="1") ~uuid ~symbol ~side ~price ~qty t =
  let side = match side with `Buy -> "1" | `Sell -> "2" in
  create t NewOrderSingle [
    HandlInst, "1";
    ClOrdID, uuid;
    Symbol, coinbase_of_symbol symbol;
    Side, side;
    Price, Printf.sprintf "%f" price;
    OrderQty, Printf.sprintf "%f" qty;
    OrdType, order_type;
    TimeInForce, tif;
  ]
