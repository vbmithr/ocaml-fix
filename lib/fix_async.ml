open Core.Std
open Async.Std

open Fix

let log = Log.(create ~level:`Debug ~output:[Output.(stderr ())]
                 ~on_error:`Raise)

let with_connection
    ?(timeout=Time.Span.(of_int_sec 2))
    ?(max_msg_size=4096)
    ?(ssl=false)
    ~host ~port ~username ~passwd () =
  let client_read, msg_write = Pipe.create () in
  let msg_read, client_write = Pipe.create () in
  let run s r w =
    let scratchbuf = String.create max_msg_size in
    let handle_chunk buf ~pos ~len =
      String.fill scratchbuf '\000' ~pos:0 ~len:max_msg_size;
      Bigstring.To_string.blit buf pos scratchbuf 0 len;
      let msg = read_msg scratchbuf 0 len in
      Log.debug log "<- %s\n%!" @@ show_msg msg;
      Pipe.write msg_write msg >>| fun () ->
      `Consumed (len, `Need_unknown)
    in
    don't_wait_for @@
    Pipe.transfer msg_read Writer.(pipe w)
      ~f:(fun msg ->
          Log.debug log "-> %s\n" @@ show_msg msg;
          string_of_msg msg);
    Reader.read_one_chunk_at_a_time r handle_chunk >>| function
    | `Eof -> Ok ()
    | `Stopped v -> Ok ()
    | `Eof_with_unconsumed_data rem -> Ok ()
  in
  let f s r w =
    let is_closed =
      Reader.close_finished r >>| fun () ->
      Ok () in
    (if ssl then
      Conduit_async_ssl.ssl_connect r w
    else
      return (r, w))
    >>= fun (r, w) ->
    Deferred.any [ is_closed; run s r w ]
  in
  don't_wait_for @@
  (Tcp.(with_connection ~timeout
          (to_host_and_port host port) f) >>| fun _ ->
   Log.debug log "TCP connection terminated, exiting");
  return @@ (client_read, client_write)

