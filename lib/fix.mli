type msgname =
  | Heartbeat
  | Logon
  | Logout

val msgtype_of_msgname : msgname -> string
val msgname_of_msgtype : string -> msgname option

type fieldname =
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

type field = {
  tag: int;
  value: string
} [@@deriving show,create]

type msg = field list [@@deriving show]

val field_of_string : string -> field
val string_of_field : field -> string

val body_length : field list -> int

val msg_maker : ?major:int -> ?minor:int ->
  sendercompid:string ->
  targetcompid:string -> unit ->
  (string -> field list -> int * field list)

val string_of_msg : field list -> string
val read_msg : bytes -> pos:int -> len:int -> field list
val write_msg : field list -> bytes -> pos:int -> unit
