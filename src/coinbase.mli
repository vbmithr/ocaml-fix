type t

val init : ?seq:int -> string -> t

val logon : ?heartbeat:int -> secret:string -> passphrase:string -> t -> Fix.t
val logout : t -> Fix.t
val heartbeat : ?testreqid:string -> t -> Fix.t

val new_order :
  ?order_type:string ->
  ?tif:string ->
  uuid:string ->
  symbol:string ->
  side:[< `Buy | `Sell ] ->
  price:float -> qty:float -> t -> Fix.t
