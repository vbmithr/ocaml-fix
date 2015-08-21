open Fix_intf

val string_of_msgname : msgname -> string
val msgname_of_string : string -> msgname option


module Msg : sig
  type t
  val show : t -> string

  val find : t -> int -> string option
  val iter : (int -> string -> unit) -> t -> unit
end

val field_of_string : string -> int * string
val string_of_field : int -> string -> string

val body_length : Msg.t -> int

val msg_maker : ?major:int -> ?minor:int ->
  sendercompid:string ->
  targetcompid:string -> unit ->
  (string -> (int * string) list -> int * Msg.t)

val add_field : Msg.t -> int -> string -> Msg.t

val string_of_msg : Msg.t -> string
val read_msg : bytes -> pos:int -> len:int -> Msg.t
val write_msg : bytes -> pos:int -> Msg.t -> unit
