open! Core

module Def : sig
  type t =
    | Arg of string
    | Instr of string * int * Bril.Instr.t
  [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
end

include Dataflow.S with type t := Def.Set.t String.Map.t
