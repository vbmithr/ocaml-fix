val logon :
  ?cancel_on_disconnect:bool ->
  ?heartbeat:Ptime.Span.t ->
  username:string ->
  secret:string ->
  ts:Ptime.t ->
  int ->
  Fix.t

val logout : int -> Fix.t
val heartbeat : string -> int -> Fix.t
