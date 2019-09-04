open Fix
open Fixtypes

val tid : string
val url : Uri.t

val logon_fields :
  ?cancel_on_disconnect:[<`All | `Session] ->
  key:string ->
  secret:string ->
  logon_ts:Ptime.t -> Field.t list

val testreq : testreqid:string -> t
val order_status_request : ?orderID:string -> unit -> t
val new_order :
  ?reduceOnly:bool -> side:Side.t -> price:float -> qty:float ->
  timeInForce:TimeInForce.t -> symbol:string -> Uuidm.t -> t

val cancel_order : [<`ClOrdID of Uuidm.t | `OrderID of int64 ] -> t
