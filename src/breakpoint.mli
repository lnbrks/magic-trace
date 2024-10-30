open! Core
open Async

module Hit : sig
  type t =
    { timestamp : Time_ns.Span.t
    ; passed_timestamp : Time_ns.Span.t
    ; passed_val : int
    ; tid : Pid.t
    ; ip : int64
    }
  [@@deriving sexp]
end

module Perf_bp : sig
  type t

  (** Uses [perf_event_open] to set a hardware breakpoint at a given address in a process.
      When that breakpoint is hit the resulting file descriptor will poll as readable.

      If [single_hit] is set the breakpoint will disable itself after being hit once.

      If [inherit_behavior] is [`Inherit_and_signal_this_thread], then the event will be
      created with the inherit attribute set, which means it follows thread creation and
      forking. Further, in this case, polls to the resulting fd will not behave as expected
      due to a kernel bug.

      Instead, the fd is setup to notify via signal delivery. Use the [Signal_delivery]
      module to get notified of this using a signalfd. *)
  val create
    :  [ `Tid of Pid.t | `Cpu of int ]
    -> addr:int64
    -> single_hit:bool
    -> t Or_error.t

  val destroy : t -> unit
  val next_hit : t -> Hit.t option

  (** Returns a waitable fd valid only until [t] is destroyed or GCd *)
  val fd : t -> Core_unix.File_descr.t
end

module All_cpus : sig
  val with_process_filtering__resolve_when_done
    :  Pid.t
    -> addr:int64
    -> single_hit:bool
    -> interrupt:unit Deferred.t
    -> on_async_cycle_with_hit:(Hit.t -> unit)
    -> unit Deferred.t Or_error.t
end
