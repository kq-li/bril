open! Core
open! Async

let optimize (func : Bril.Func.t) =
  let blocks =
    Map.map func.blocks ~f:(fun block ->
        List.foldi block ~init:(String.Map.empty, 0, []) ~f:(fun i (defs, spec_i, instrs) ->
          function
          | Bril.Instr.Speculate as instr -> (defs, i, instr :: instrs)
          | Bril.Instr.Guard (arg, _) as instr ->
            let def_i = Map.find defs arg |> Option.value ~default:spec_i in
            let (l1, l2) = List.split_n instrs (List.length instrs - def_i - 1) in
            (defs, spec_i, l1 @ (instr :: l2))
          | instr ->
            ( ( match Bril.Instr.dest instr with
              | None -> defs
              | Some (x, _) -> Map.set defs ~key:x ~data:i ),
              spec_i,
              instr :: instrs ))
        |> Tuple.T3.get3
        |> List.rev)
  in
  { func with blocks }

let transform trace (funcs : Bril.t) =
  let (trace, metadata) = List.split_n trace (List.length trace - 2) in
  let recover = "__recover" in
  let end_trace = "__end_trace" in
  match metadata with
  | [ name; last_index ] ->
    let last_index = Int.of_string last_index in
    let instrs =
      List.concat_map trace ~f:(fun line ->
          let open Yojson.Basic.Util in
          let json = Yojson.Basic.from_string line in
          match json |> member "instr" |> Bril.Instr.of_json with
          | Jmp _ -> []
          | Br (arg, _, _) ->
            if json |> member "branch" |> to_bool then [ Bril.Instr.Guard (arg, recover) ]
            else
              let arg' = "__not_" ^ arg in
              [
                Unary ((arg', Bril.Bril_type.BoolType), Bril.Op.Unary.Not, arg);
                Guard (arg', recover);
              ]
          | instr -> [ instr ])
    in
    List.map funcs ~f:(fun func -> (func.name, func))
    |> String.Map.of_alist_exn
    |> Fn.flip Map.update name ~f:(function
           | None -> failwith (name ^ " function not found")
           | Some func ->
             let (l1, l2) = List.split_n (Bril.Func.instrs func) last_index in
             Bril.Func.set_instrs func (l1 @ (Label end_trace :: l2)))
    |> Fn.flip Map.update "main" ~f:(function
           | None -> failwith "main function not found"
           | Some func ->
             let (label, orig_instrs) = List.split_n (Bril.Func.instrs func) 1 in
             Bril.Func.set_instrs
               func
               (List.concat
                  [
                    label;
                    [ Speculate ];
                    instrs;
                    [ Commit; Jmp end_trace; Label recover ];
                    orig_instrs;
                  ])
             |> optimize)
    |> Map.data
  | _ -> failwith "invalid trace"

let () =
  Command.async
    ~summary:"use input trace to optimize program"
    (let%map_open.Command filename = anon ("filename" %: string) in
     fun () ->
       let trace = In_channel.input_lines In_channel.stdin in
       let%map.Deferred bril =
         Process.run_exn ~prog:"bril2json" ~args:[] ~stdin:(In_channel.read_all filename) ()
       in
       let funcs = Yojson.Basic.from_string bril |> Bril.from_json in
       transform trace funcs |> Bril.to_json |> Yojson.Basic.pretty_to_string |> print_endline)
  |> Command.run
