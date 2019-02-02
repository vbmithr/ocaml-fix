open Fix

val sid : string
val tid : string

val logon_fields :
  ?cancel_on_disconnect:bool ->
  username:string ->
  secret:string ->
  ts:Ptime.t -> Field.t list
