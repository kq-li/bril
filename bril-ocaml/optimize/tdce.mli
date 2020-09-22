open! Core

module Global : sig
  val process : Bril.Instr.t list -> Bril.Instr.t list
end

module Local : sig
  val process : Bril.Instr.t list -> Bril.Instr.t list
end
