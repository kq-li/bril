open! Core
open! Common
module Bril_type = Bril_type
module Common = Common
module Func = Func
module Instr = Instr
module Op = Op

type t = Func.t list [@@deriving compare, sexp_of]

val from_json : Yojson.Basic.t -> t
val to_json : t -> Yojson.Basic.t
