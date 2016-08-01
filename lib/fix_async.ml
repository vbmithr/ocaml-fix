open Core.Std
open Async.Std

open Bs_devkit.Core

let with_connection
  ?log
  ?(timeout=Time.Span.(of_int_sec 2))
  ?(tmpbuf=String.create 4096)
  ?tls
  ~host ~port () =
  let client_read, msg_write = Pipe.create () in
  let msg_read, client_write = Pipe.create () in
  let run s r w =
    let handle_chunk msgbuf ~pos ~len =
      if len > String.length tmpbuf then failwith "Message bigger than tmpbuf";
      Bigstring.To_string.blit msgbuf pos tmpbuf 0 len;
      let msg = Fix.read tmpbuf ~len in
      maybe_debug log "<- %s" (Fix.sexp_of_t msg |> Sexplib.Sexp.to_string);
      Pipe.write msg_write msg >>| fun () ->
      `Consumed (len, `Need_unknown)
    in
    don't_wait_for @@
    Pipe.transfer msg_read Writer.(pipe w) ~f:(fun msg ->
        maybe_debug log "-> %s" (Fix.sexp_of_t msg |> Sexplib.Sexp.to_string);
        Fix.to_bytes msg);
    Reader.read_one_chunk_at_a_time r handle_chunk
  in
  let tcp_f s r w =
    begin match tls with
    | None -> return (r, w)
    | Some `Noconf -> Conduit_async_ssl.ssl_connect r w
    | Some (`CAFile ca_file) -> Conduit_async_ssl.ssl_connect ~ca_file r w
    end >>= fun (r, w) -> run s r w
  in
  don't_wait_for begin
    Tcp.(with_connection ~timeout (to_host_and_port host port) tcp_f) >>| fun _ ->
    maybe_info log "TCP connection terminated";
    Pipe.close client_write
  end;
  return (client_read, client_write)
