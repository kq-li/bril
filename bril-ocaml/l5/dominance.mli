open! Core

module type S = sig
  type t

  val dominators : ?strict:bool -> Bril.Func.t -> t String.Map.t
  val dominated : ?strict:bool -> Bril.Func.t -> t String.Map.t
  val tree : Bril.Func.t -> t String.Map.t * t String.Map.t
  val frontier : Bril.Func.t -> t String.Map.t
end

module Sets : S with type t := String.Set.t
module Lists : S with type t := string list
