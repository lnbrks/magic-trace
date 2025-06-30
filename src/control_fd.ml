open! Core
open! Async

module Cmd = struct
  (* Commands are single characters, optionally separated by \n *)
  let enable = 'e'
  let ack = 'a'
  let ready = 'r'
end

let max_cmds_per_cycle = 8
let ack_str = Char.to_string Cmd.ack ^ "\n"
let ready_str = Char.to_string Cmd.ready ^ "\n"
let ack_buf_len = max_cmds_per_cycle * String.length ack_str

type t =
  { ctl_rx : Core_unix.File_descr.t
  ; ack_tx : Async_unix.Writer.t
  ; cmd_read_buf : Bytes.t
  }

let create ~ctl_rx ~ack_tx =
  let ack_tx =
    Async_unix.Fd.create Async_unix.Fd.Kind.File ack_tx (Info.of_string "Control-Ack-TX")
    |> Async_unix.Writer.create ~raise_when_consumer_leaves:true ~buf_len:ack_buf_len
  in
  let cmd_read_buf = Bytes.make (max_cmds_per_cycle * 2) '\000' in
  { ctl_rx; ack_tx; cmd_read_buf }
;;

let assert_fifo fd =
  let%tydi { st_kind; _ } = Core_unix.fstat fd in
  match st_kind with
  | S_FIFO -> ()
  | _ -> failwith "Expected controlfd to be FIFO"
;;

let of_string_fd_list str =
  let fds =
    String.split str ~on:',' |> List.map ~f:String.strip |> List.map ~f:Int.of_string
  in
  match fds with
  | [ ctl_rx; ack_tx ] ->
    (* Setting CLOEXEC validates the fds *)
    let ctl_rx = Core_unix.File_descr.of_int ctl_rx in
    let ack_tx = Core_unix.File_descr.of_int ack_tx in
    Core_unix.set_close_on_exec ctl_rx;
    Core_unix.set_close_on_exec ack_tx;
    assert_fifo ctl_rx;
    assert_fifo ack_tx;
    create ~ctl_rx ~ack_tx
  | _ -> failwith "Expected exactly two control fds"
;;

let write_to_ack_fd t str = Async_unix.Writer.write t.ack_tx str
let notify_ready t = write_to_ack_fd t ready_str

let on_command_ready t ~on_enable =
  (* Async fd creation set nonblock already *)
  let bytes_read = Core_unix.read_assume_fd_is_nonblocking t.ctl_rx t.cmd_read_buf in
  if bytes_read = 0
  then (* TODO ivar? *) ()
  else (
    let num_acks = ref 0 in
    for i = 0 to bytes_read - 1 do
      match Bytes.get t.cmd_read_buf i with
      | c when Char.( = ) c Cmd.enable ->
        on_enable ();
        incr num_acks
      | _ -> (* ignore *) ()
    done;
    Bytes.fill t.cmd_read_buf ~pos:0 ~len:(Bytes.length t.cmd_read_buf) '\000';
    for _ = 1 to !num_acks do
      write_to_ack_fd t ack_str
    done)
;;

let begin_listening t ~on_enable =
  let async_ctl_rx =
    Async_unix.Fd.create Async_unix.Fd.Kind.File t.ctl_rx (Info.of_string "Control-RX")
  in
  don't_wait_for
    (match%map
       Async_unix.Fd.every_ready_to
         async_ctl_rx
         `Read
         (fun () -> on_command_ready t ~on_enable)
         ()
     with
     | `Bad_fd | `Closed | `Unsupported ->
       failwith "Failed to listen to magic trace control fd")
;;

let param =
  Command.Param.(
    flag
      "controlfd"
      (optional (Arg_type.create of_string_fd_list))
      ~doc:
        " Pass two, comma-separated open FIFO file descriptors for commands and \
         acknowledgements, respectively. When provided, magic-trace will attach to the \
         running program, but not enable tracing until an enable command is received.")
;;
