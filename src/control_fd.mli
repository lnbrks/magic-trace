open! Core
open! Async

type t

val notify_ready : t -> unit
val begin_listening : t -> on_enable:(unit -> unit) -> unit
val param : t option Command.Param.t
