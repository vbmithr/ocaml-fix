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

let logout () =
  make_msg "5" [8500, "0"]

let heartbeat ?testreqid ~username ~passwd () =
  let fields =
    (553, username) :: (554, passwd) ::
    (match testreqid with None -> [] | Some value -> [112, value]) in
  make_msg "0" fields

let testreq reqid = make_msg "1" [112, string_of_int reqid]

let account_info_request ?account_id ~username ~passwd reqid =
  let fields =
    (8000, reqid) :: (match account_id with None -> [] | Some id -> [1, id]) in
  make_msg "Z1000" fields
