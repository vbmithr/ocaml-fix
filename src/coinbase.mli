open Fix

val init : ?seq:int -> string -> Factory.t

val logon : ?heartbeat:int -> secret:string -> passphrase:string -> Factory.t -> t
val logout : Factory.t -> t
val heartbeat : ?testreqid:string -> Factory.t -> t

val new_order :
  ?order_type:string ->
  ?tif:string ->
  uuid:string ->
  symbol:string ->
  side:[< `Buy | `Sell ] ->
  price:float -> qty:float -> Factory.t -> t
