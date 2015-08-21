open Fix_intf

val string_of_msgname : msgname -> string
val msgname_of_string : string -> msgname option

type msg

val show_msg : msg -> string

val find_field : msg -> int -> string option

val field_of_string : string -> int * string
val string_of_field : int -> string -> string

val body_length : msg -> int

val msg_maker : ?major:int -> ?minor:int ->
  sendercompid:string ->
  targetcompid:string -> unit ->
  (string -> (int * string) list -> int * msg)

val add_field : msg -> int -> string -> msg

val string_of_msg : msg -> string
val read_msg : bytes -> pos:int -> len:int -> msg
val write_msg : bytes -> pos:int -> msg -> unit
