open! Core

type dir =
  | Forward
  | Backward

module type M_intf = sig
  type t [@@deriving compare, sexp_of]

  val analysis : string
  val dir : dir
  val top : t
  val meet : t -> t -> t
  val init : Bril.Func.t -> t
  val transfer : t -> Bril.Instr.t list -> t
  val to_string : t -> string

  include Comparable.S_plain with type t := t
end

module type S = sig
  type t

  val analyze : Bril.Func.t -> t String.Map.t * t String.Map.t
  val print : Bril.Func.t -> unit
end

module Make (M : M_intf) : S with type t := M.t
