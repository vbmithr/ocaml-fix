include module type of Fix_intf

type t = private {
  major: int;
  minor: int;
  len: int;
  typ: MsgType.t;
  fields: string Tag.Map.t
} [@@deriving sexp]

val make_create :
  ?major:int ->
  ?minor:int ->
  sendercompid:string ->
  targetcompid:string -> unit ->
  MsgType.t -> (Tag.t * string) list ->
  int * t

val add_field : t -> Tag.t -> string -> t

val read : ?pos:int -> ?len:int -> string -> t
val to_bytes : t -> string
