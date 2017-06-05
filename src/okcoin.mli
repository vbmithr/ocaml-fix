open Fix

val logon :
  ?heartbeat:int -> username:string -> passwd:string -> unit -> t

val logout : ?response:bool -> unit -> t

val heartbeat :
  ?testreqid:string -> username:string -> passwd:string -> unit -> t

val testreq : string -> t

val account_info_request : ?account_id:string -> string -> t
