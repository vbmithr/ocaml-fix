open Core

val logon :
  ?cancel_on_disconnect:bool ->
  ?heartbeat:Time_ns.Span.t ->
  secret:string -> unit ->
  Fix.t

val logout : int -> Fix.t

val heartbeat : string -> int -> Fix.t
