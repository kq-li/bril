open! Core

module Value = struct
  module T = struct
    type t =
      | Binary of Bril.Op.Binary.t * int * int
      | Unary of Bril.Op.Unary.t * int
      | Const of Bril.Const.t
      | Call
    [@@deriving compare, sexp_of]

    let canonicalize = function
      | Binary (binop, n1, n2) when n1 > n2 -> Binary (binop, n2, n1)
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
        (* print_s
         *   [%message
         *     (rows_by_num : (int * Value.t * string) Int.Map.t)
         *       (nums_by_var : int String.Map.t)
         *       (nums_by_value : int Value.Map.t)
         *       (instr : Bril.Instr.t)];
         * print_endline ""; *)
        let row_of_num = Map.find rows_by_num in
        let row_of_num_exn = Map.find_exn rows_by_num in
        let num_of_var = Map.find nums_by_var in
        let num_of_value = Map.find nums_by_value in
        let dest_and_value =
          let open Option.Let_syntax in
          match instr with
          | Const (dest, const) -> Some (dest, Value.Const const)
          | Unary (dest, unop, arg) ->
            let%map num = num_of_var arg in
            (dest, Value.Unary (unop, num))
          | Binary (dest, binop, arg1, arg2) ->
            let%bind num1 = num_of_var arg1 in
            let%map num2 = num_of_var arg2 in
            (dest, Value.Binary (binop, num1, num2))
          | Call (dest, _, _) -> Option.map dest ~f:(fun dest -> (dest, Value.Call))
          | _ -> None
        in
        let extract_const num =
          match%bind.Option row_of_num num with
          | (_, Value.Const const, _) -> Some const
          | _ -> None
        in
        let fold_value value : Value.t =
          match value with
          | Value.Unary (unop, orig_num) ->
            ( match (extract_const orig_num, unop) with
            | (Some const, Id) -> Const const
            | (Some (Bool b), Not) -> Const (Bool (not b))
            | _ -> value )
          | Value.Binary (binop, orig_num1, orig_num2) ->
            let fold_ints n1 n2 : Bril.Const.t =
              match binop with
              | Add -> Int (n1 + n2)
              | Mul -> Int (n1 * n2)
              | Sub -> Int (n1 - n2)
              | Div -> Int (n1 / n2)
              | Eq -> Bool (n1 = n2)
              | Lt -> Bool (n1 < n2)
              | Gt -> Bool (n1 > n2)
              | Le -> Bool (n1 <= n2)
              | Ge -> Bool (n1 >= n2)
              | _ ->
                failwithf
                  "invalid binop %s on ints"
                  Bril.Op.Binary.(List.Assoc.find_exn by_op binop ~equal)
                  ()
            in
            let fold_bools b1 b2 : Bril.Const.t =
              match binop with
              | And -> Bool (b1 && b2)
              | Or -> Bool (b1 || b2)
              | _ ->
                failwithf
                  "invalid binop %s on bools"
                  Bril.Op.Binary.(List.Assoc.find_exn by_op binop ~equal)
                  ()
            in
            ( match (extract_const orig_num1, extract_const orig_num2) with
            | (Some (Int n1), Some (Int n2)) -> Const (fold_ints n1 n2)
            | (Some (Bool b1), Some (Bool b2)) -> Const (fold_bools b1 b2)
            | _ -> value )
          | _ -> value
        in
        let replace_var var =
          (let open Option.Let_syntax in
          let%bind num = num_of_var var in
          let%map (_, _, orig_var) = row_of_num num in
          orig_var)
          |> Option.value ~default:var
        in
        let replaced_instr : Bril.Instr.t =
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
        let make_new_instr dest value : Bril.Instr.t =
          match (value, num_of_value value) with
          | (Value.Const const, _) -> Const (dest, const)
          | (_, Some orig_num) ->
            let (_, _, orig_var) = row_of_num_exn orig_num in
            Unary (dest, Bril.Op.Unary.Id, orig_var)
          | (Value.Unary (unop, num), _) ->
            let (_, _, var) = row_of_num_exn num in
            Unary (dest, unop, replace_var var)
          | (Value.Binary (binop, num1, num2), _) ->
            let (_, _, var1) = row_of_num_exn num1 in
            let (_, _, var2) = row_of_num_exn num2 in
            Binary (dest, binop, replace_var var1, replace_var var2)
          | _ -> replaced_instr
        in
        let add_row value var instr =
          let num = Map.length rows_by_num in
          let nums_by_value =
            match value with
            | Value.Call ->
              (* maybe todo: eliminate redundant pure function calls here? *)
              nums_by_value
            | value -> Map.set nums_by_value ~key:value ~data:num
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
          (rows_by_num, nums_by_var, nums_by_value, replaced_instr :: block)
        | Some (((var, _) as dest), value) ->
          let value = value |> fold_value |> Value.canonicalize in
          let new_instr = make_new_instr dest value in
          ( match (value, num_of_value value) with
          | (Unary (Bril.Op.Unary.Id, orig_num), _)
          | (_, Some orig_num) ->
            skip_row orig_num var new_instr
          | (_, None) -> add_row value var new_instr ))
  in
  List.rev block
