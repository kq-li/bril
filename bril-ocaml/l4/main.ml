open! Core

let analyze ({ args; blocks; order; preds; succs; _ } : Bril.Func.t) =
  let transfer b in_b =
    Map.find_exn blocks b
    |> List.fold ~init:in_b ~f:(fun defs instr ->
           match Bril.Instr.dest instr with
           | None -> defs
           | Some (name, _) -> Set.add defs name)
  in
  let rec aux worklist ins outs =
    match Fqueue.dequeue worklist with
    | None -> (ins, outs)
    | Some (b, worklist) ->
      let in_b =
        Map.find_exn preds b
        |> List.fold ~init:(Map.find_exn ins b) ~f:(fun in_b pred ->
               Set.union in_b (Map.find_exn outs pred))
      in
      let out_b = transfer b in_b in
      let worklist =
        if String.Set.equal out_b (Map.find_exn outs b) then worklist
        else Map.find_exn succs b |> List.fold ~init:worklist ~f:Fqueue.enqueue
      in
      aux worklist (Map.set ins ~key:b ~data:in_b) (Map.set outs ~key:b ~data:out_b)
  in
  let empty = List.map order ~f:(fun b -> (b, String.Set.empty)) |> String.Map.of_alist_exn in
  let (ins, outs) =
    aux
      (Fqueue.of_list order)
      (Map.set empty ~key:(List.hd_exn order) ~data:(List.map args ~f:fst |> String.Set.of_list))
      empty
  in
  let string_of_set = Fn.compose (String.concat ~sep:", ") Set.to_list in
  List.iter order ~f:(fun b ->
      printf
        "%s\n  in: {%s}\n  out: {%s}\n"
        b
        (Map.find_exn ins b |> string_of_set)
        (Map.find_exn outs b |> string_of_set))

let () =
  In_channel.input_all In_channel.stdin
  |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.iter ~f:analyze
