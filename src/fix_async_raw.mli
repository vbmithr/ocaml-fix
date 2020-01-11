open Async
open Fix

module Log_async : Logs_async.LOG

val connect : Uri.t -> (t Pipe.Reader.t * t Pipe.Writer.t) Deferred.t
