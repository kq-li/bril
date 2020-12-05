open! Core

module Maybe_const : sig
  type t =
    | Const of Bril.Const.t
    | Dynamic
  [@@deriving compare, equal, sexp_of]

  include Comparable.S_plain with type t := t
end

include Dataflow.S with type t := Maybe_const.t String.Map.t
