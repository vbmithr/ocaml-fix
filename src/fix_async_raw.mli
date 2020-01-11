open Async
open Fix

module Log_async : Logs_async.LOG

val connect :
  ?stream:Faraday.t -> Uri.t ->
  (t Pipe.Reader.t * t Pipe.Writer.t) Deferred.Or_error.t
