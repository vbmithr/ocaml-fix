include module type of Fix_intf

type t = private {
  major: int;
  minor: int;
  len: int;
  typ: MsgType.t;
  fields: string Tag.Map.t
}

(* val field_of_string : string -> int * string *)
(* val string_of_field : int -> string -> string *)

(* val body_length : t -> int *)

val msg_maker :
  ?now:float ->
  ?major:int ->
  ?minor:int ->
  sendercompid:string ->
  targetcompid:string -> unit ->
  (MsgType.t -> (Tag.t * string) list -> int * t)

val add_field : t -> Tag.t -> string -> t

val to_string : t -> string
val read : ?pos:int -> ?len:int -> string -> t
(* val write : bytes -> pos:int -> t -> unit *)
