open Core.Std
open Async.Std

val with_connection :
  ?timeout:Time.Span.t ->
  ?max_msg_size:int ->
  ?tls:bool -> host:string -> port:int ->
  unit ->
  (Fix.Msg.t Pipe.Reader.t * Fix.Msg.t Pipe.Writer.t) Deferred.t
