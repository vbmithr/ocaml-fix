open Rresult
open Core
open Async
open Fix

open Bs_devkit

let src = Logs.Src.create "fix.async"

let with_connection
  ?(tmpbuf=Bytes.create 4096)
  ~version uri =
  let client_read, msg_write = Pipe.create () in
  let msg_read, client_write = Pipe.create () in
  let run (r, w) =
    let handle_chunk msgbuf ~pos ~len =
      if len > Bytes.length tmpbuf then
        failwith "Message bigger than tmpbuf" ;
      Bigstring.To_bytes.blit msgbuf pos tmpbuf 0 len;
      let msg_str =
        Bytes.unsafe_to_string
          ~no_mutation_while_string_reachable:tmpbuf in
      begin match Fix.read msg_str ~len with
      | Error msg ->
        Logs_async.err ~src begin fun m ->
          m "<- Invalid message received (%a): %s" R.pp_msg msg msg_str
        end
      | Ok msg ->
        Logs_async.debug ~src begin fun m ->
          m "<- %a" pp msg
        end >>= fun () ->
        Pipe.write msg_write msg
      end >>| fun () ->
      `Consumed (len, `Need_unknown)
    in
    don't_wait_for @@
    Pipe.transfer msg_read Writer.(pipe w) ~f:begin fun msg ->
      Logs.debug ~src (fun m -> m "-> %a" pp msg) ;
      Fix.to_bytes ~version msg
    end ;
    Reader.read_one_chunk_at_a_time r handle_chunk
  in
  don't_wait_for begin
    addr_of_uri uri >>= fun addr ->
    Monitor.try_with_or_error begin fun () ->
      Conduit_async.V2.connect addr >>=
      run >>= fun _ ->
      (* TODO: cases *)
      Deferred.unit
    end >>= function
    | Error e ->
      Logs_async.err ~src (fun m -> m "%a" Error.pp e)
    | Ok _ ->
      Logs_async.info ~src
        (fun m -> m "TCP connection terminated") >>= fun () ->
      Pipe.close client_write ;
      Deferred.unit
    end;
  return (client_read, client_write)
