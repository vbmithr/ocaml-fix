type message =
  | Heartbeat
  | Logon
  | Logout

val msgtype_of_message : message -> string
val message_of_msgtype : string -> message option

type field =
  | BeginString [@value 8]
  | BodyLength [@value 9]
  | CheckSum [@value 10]
  | SenderCompId [@value 49]
  | TargetCompId [@value 56]
  | Text [@value 58]
  | HeartBtInt [@value 108]
  | ResetSeqNumFlag [@value 141]
  | Username [@value 553]
  | Password [@value 554]
      [@@deriving enum]

type atom = {
  tag: int;
  value: string
}

val atom_of_string : string -> atom
val msg_of_string : string -> atom list
