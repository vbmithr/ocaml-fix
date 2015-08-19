open Fix

let make_msg = msg_maker
    ~sendercompid:"de5bbcf9-a6fc-4c32-b569-7d4d1d619525"
    ~targetcompid:"OKSERVER" ()

let logon ?(heartbeat=30) ~username ~passwd () =
  let fields =
    [
     98, "0"; (* encryption *)
     108, string_of_int heartbeat;
     553, username;
     554, passwd;
    ] in
  make_msg "A" fields

let logout ?(response=false) () =
  make_msg "5" (if response then [8500, "0"] else [])

let heartbeat ?testreqid ~username ~passwd () =
  let fields =
    (553, username) :: (554, passwd) ::
    (match testreqid with None -> [] | Some value -> [112, value]) in
  make_msg "0" fields

let testreq reqid = make_msg "1" [112, reqid]

let account_info_request ?account_id reqid =
  let fields =
    (8000, reqid) :: (match account_id with None -> [] | Some id -> [1, id]) in
  make_msg "Z1000" fields

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
  let fields = [
    146, "1"; (* max one symbol allowed by OKCoin. *)
    55, string_of_symbol symbol;
    262, reqid;
    263, string_of_operation operation;
    264, string_of_int depthlvls;
    265, string_of_updatetype updatetype;
    267, string_of_int @@ List.length entrytypes;
  ] @
    List.map (fun e -> 269, string_of_entrytype e) entrytypes
  in
  make_msg "V" fields

let incremental_trades ~symbol reqid =
  market_data_request ~symbol ~operation:`Snapsub ~updatetype:`Incremental
    ~entrytypes:[`Trade] reqid
