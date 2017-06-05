open Fix

let t = Factory.create
    ~sendercompid:"de5bbcf9-a6fc-4c32-b569-7d4d1d619525"
    ~targetcompid:"OKSERVER" ()

let logon ?(heartbeat=30) ~username ~passwd () =
  let fields =
    Tag.[
      EncryptMethod, "0"; (* encryption *)
      HeartBtInt, string_of_int heartbeat;
      Username, username;
      Password, passwd;
    ] in
  create t Logon fields

let logout ?(response=false) () =
  create t Logout (if response then [Unknown 8500, "0"] else [])

let heartbeat ?testreqid ~username ~passwd () =
  let fields =
    (Tag.Username, username) :: (Tag.Password, passwd) ::
    (match testreqid with None -> [] | Some value -> [TestReqID, value]) in
  create t Heartbeat fields

let testreq reqid =
  create t TestRequest [TestReqID, reqid]

let account_info_request ?account_id reqid =
  let fields =
    (Tag.Unknown 8000, reqid) :: (match account_id with None -> [] | Some id -> [Tag.Account, id]) in
  create t (Unknown "Z1000") fields

let orders_request ~start_id ~status ~symbol reqid =
  let string_of_status = function
    | `Not_filled -> "0"
    | `Fully_filled -> "1" in
  let string_of_symbol = function
    | `XBTUSD -> "BTC/USD"
    | `LTCUSD -> "LTC/USD" in
  let fields =
    Tag.[
      OrderID, start_id;
      OrdStatus, string_of_status status;
      Symbol, string_of_symbol symbol;
      TradeRequestID, reqid;
    ] in
  create t (Unknown "Z2000") fields

let market_data_request ?(depthlvls=1) ~symbol ~operation ~updatetype ~entrytypes reqid =
  let string_of_symbol = function
    | `XBTUSD -> "BTC/USD"
    | `LTCUSD -> "LTC/USD" in
  let string_of_operation = function
    | `Snapshot -> "0"
    | `Snapsub -> "1"
    | `Unsubscribe -> "2" in
  let string_of_updatetype = function
    | `Full -> "0"
    | `Incremental -> "1" in
  let string_of_entrytype = function
    | `Bid -> "0"
    | `Ask -> "1"
    | `Trade -> "2"
    | `O -> "4"
    | `H -> "7"
    | `L -> "8"
    | `C -> "5"
    | `VWAP -> "9"
    | `Vol -> "B" in
  let fields = Tag.[
    NoRelatedSym, "1"; (* max one symbol allowed by OKCoin. *)
    Symbol, string_of_symbol symbol;
    MDReqID, reqid;
    SubscriptionRequestType, string_of_operation operation;
    MarketDepth, string_of_int depthlvls;
    MDUpdateType, string_of_updatetype updatetype;
    NoMDEntryTypes, string_of_int @@ List.length entrytypes;
  ] @
    List.map (fun e -> Tag.MDEntryType, string_of_entrytype e) entrytypes
  in
  create t MarketDataRequest fields

let incremental_trades ~symbol reqid =
  market_data_request ~symbol ~operation:`Snapsub ~updatetype:`Incremental
    ~entrytypes:[`Trade] reqid
