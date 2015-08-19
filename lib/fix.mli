type msgname =
  | Heartbeat
  | Logon
  | Logout

val msgtype_of_msgname : msgname -> string
val msgname_of_msgtype : string -> msgname option

type tag =
  | BeginString [@value 8]
  | BodyLength [@value 9]
  | CheckSum [@value 10]
  | MsgType [@value 35]
  | SenderCompId [@value 49]
  | TargetCompId [@value 56]
  | Text [@value 58]
  | HeartBtInt [@value 108]
  | ResetSeqNumFlag [@value 141]
  | Username [@value 553]
  | Password [@value 554]
      [@@deriving enum]

type msg = (int, string) Hashtbl.t

val show_msg : msg -> string

val field_of_string : string -> int * string
val string_of_field : int -> string -> string

val body_length : msg -> int

val msg_maker : ?major:int -> ?minor:int ->
  sendercompid:string ->
  targetcompid:string -> unit ->
  (string -> (int * string) list -> int * msg)

val string_of_msg : msg -> string
val read_msg : bytes -> pos:int -> len:int -> msg
val write_msg : bytes -> pos:int -> msg -> unit
