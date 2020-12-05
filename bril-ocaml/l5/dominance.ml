open! Core

module type S = sig
  type t

  val dominators : ?strict:bool -> Bril.Func.t -> t String.Map.t
  val dominated : ?strict:bool -> Bril.Func.t -> t String.Map.t
  val tree : Bril.Func.t -> t String.Map.t * t String.Map.t
  val frontier : Bril.Func.t -> t String.Map.t
end

module Sets : S with type t := String.Set.t = struct
  let preds_to_succs preds =
    Map.fold
      preds
      ~init:(Map.map preds ~f:(const String.Set.empty))
      ~f:(fun ~key:vertex ~data:vertices succs ->
        Set.fold vertices ~init:succs ~f:(fun succs pred ->
            Map.update succs pred ~f:(function
                | None -> String.Set.singleton vertex
                | Some vertices -> Set.add vertices vertex)))

  let dominators ?(strict = false) ({ order; preds; succs; _ } : Bril.Func.t) =
    let rec postorder (visited, vertices) vertex =
      if Set.mem visited vertex then (visited, vertices)
      else
        let (visited, vertices) =
          Map.find_exn succs vertex
          |> List.fold ~init:(Set.add visited vertex, vertices) ~f:postorder
        in
        (visited, vertices @ [ vertex ])
    in
    let (_, vertices) = postorder (String.Set.empty, []) (List.hd_exn order) in
    let rec compute dom =
      let new_dom =
        List.fold vertices ~init:dom ~f:(fun dom vertex ->
            let inter =
              Map.find_exn preds vertex
              |> List.map ~f:(Map.find_exn dom)
              |> List.reduce ~f:Set.inter
              |> Option.value ~default:String.Set.empty
            in
            Map.set dom ~key:vertex ~data:(Set.add inter vertex))
      in
      if String.Map.equal String.Set.equal dom new_dom then dom else compute new_dom
    in
    let init =
      List.mapi order ~f:(fun i vertex ->
          (vertex, if i = 0 then String.Set.singleton vertex else String.Set.of_list vertices))
      |> String.Map.of_alist_exn
    in
    compute init |> if strict then Map.mapi ~f:(fun ~key ~data -> Set.remove data key) else Fn.id

  let dominated ?strict = Fn.compose preds_to_succs (dominators ?strict)

  let tree func =
    let dominators = dominators ~strict:true func in
    let preds =
      Map.map dominators ~f:(fun doms ->
          Set.fold doms ~init:doms ~f:(fun doms dom -> Set.diff doms (Map.find_exn dominators dom)))
    in
    (preds, preds_to_succs preds)

  let frontier func =
    let dominated = dominated func in
    Map.map dominated ~f:(fun dominated ->
        Set.fold dominated ~init:String.Set.empty ~f:(fun frontier vertex ->
            Map.find_exn func.succs vertex
            |> List.filter ~f:(Fn.non (Set.mem dominated))
            |> String.Set.of_list
            |> Set.union frontier))
end

module Lists : S with type t := string list = struct
  let f = Map.map ~f:String.Set.to_list
  let dominators ?strict = Fn.compose f (Sets.dominators ?strict)
  let dominated ?strict = Fn.compose f (Sets.dominated ?strict)

  let tree func =
    let (preds, succs) = Sets.tree func in
    (f preds, f succs)

  let frontier = Fn.compose f Sets.frontier
end
