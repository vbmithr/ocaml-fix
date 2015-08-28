open Fix_intf
open Fix

let make_msg = ref @@
  msg_maker ~targetcompid:"COINSETTERFIX" ~sendercompid:"" ()

let init sendercompid =
  make_msg := msg_maker ~targetcompid:"COINSETTERFIX" ~sendercompid ()

let logon ~username ~passwd =
  !make_msg (string_of_msgname Logon)
    [98, "0";
     108, "30";
     141, "Y";
     553, username;
     554, passwd;
    ]

let logout () = !make_msg (string_of_msgname Logout) []
let heartbeat () = !make_msg (string_of_msgname Heartbeat) []
let testreq id =
  !make_msg (string_of_msgname TestRequest) [112, id]
