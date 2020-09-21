open! Core

module Value = struct
  module T = struct
    type t =
      | Binary of Bril.Op.Binary.t * int * int
      | Unary of Bril.Op.Unary.t * int
      | Const of Bril.Const.t
      | Id of int
      | Call
    [@@deriving compare, sexp_of]

    let canonicalize = function
      | Binary (binop, n1, n2) when n2 > n1 -> Binary (binop, n2, n1)
      | value -> value
  end

  include T
  include Comparable.Make_plain (T)
end

let process block =
  let (_, _, _, block) =
    List.fold
      block
      ~init:(Int.Map.empty, String.Map.empty, Value.Map.empty, [])
      ~f:(fun (rows_by_num, nums_by_var, nums_by_value, block) (instr : Bril.Instr.t) ->
        let row_of_num = Map.find_exn rows_by_num in
        let num_of_var = Map.find_exn nums_by_var in
        let num_of_value_opt = Map.find nums_by_value in
        let dest_and_value =
          match instr with
          | Const (dest, const) -> Some (dest, Value.Const const)
          | Unary (dest, Bril.Op.Unary.Id, arg) -> Some (dest, Value.Id (num_of_var arg))
          | Unary (dest, unop, arg) -> Some (dest, Value.Unary (unop, num_of_var arg))
          | Binary (dest, binop, arg1, arg2) ->
            Some (dest, Value.Binary (binop, num_of_var arg1, num_of_var arg2))
          | Call (dest, _, _) -> Option.map dest ~f:(fun dest -> (dest, Value.Call))
          | _ -> None
        in
        let replace_var var =
          let (_, _, orig_var) = var |> num_of_var |> row_of_num in
          orig_var
        in
        let new_instr : Bril.Instr.t =
          match instr with
          | Binary (dest, binop, arg1, arg2) ->
            Binary (dest, binop, replace_var arg1, replace_var arg2)
          | Unary (dest, unop, arg) -> Unary (dest, unop, replace_var arg)
          | Br (arg, l1, l2) -> Br (replace_var arg, l1, l2)
          | Call (dest, f, args) -> Call (dest, f, List.map args ~f:replace_var)
          | Ret arg -> Ret (Option.map arg ~f:replace_var)
          | Print args -> Print (List.map args ~f:replace_var)
          | _ -> instr
        in
        let add_row value var instr =
          let num = Map.length rows_by_num in
          let nums_by_value =
            match value with
            | Value.Call ->
              (* maybe todo: eliminate redundant pure function calls here? *)
              nums_by_value
            | value -> Map.set nums_by_value ~key:(Value.canonicalize value) ~data:num
          in
          ( Map.set rows_by_num ~key:num ~data:(num, value, var),
            Map.set nums_by_var ~key:var ~data:num,
            nums_by_value,
            instr :: block )
        in
        let skip_row orig_num var instr =
          (rows_by_num, Map.set nums_by_var ~key:var ~data:orig_num, nums_by_value, instr :: block)
        in
        match dest_and_value with
        | None ->
          (* don't add to table, replace instr args and add instr to block *)
          (rows_by_num, nums_by_var, nums_by_value, new_instr :: block)
        | Some ((var, _), Id orig_num) -> skip_row orig_num var new_instr
        | Some (((var, _) as dest), value) ->
          ( match num_of_value_opt value with
          | None -> add_row value var new_instr
          | Some orig_num ->
            let (_, _, orig_var) = row_of_num orig_num in
            skip_row orig_num var (Unary (dest, Bril.Op.Unary.Id, orig_var)) ))
  in
  List.rev block
