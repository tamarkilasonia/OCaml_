(* Check if two lists are connected using comparison function c *)
let connected c l1 l2 =
  List.exists (fun x ->
    List.exists (fun y -> c x y = 0) l2
  ) l1

(* Merge two lists without duplicates *)
let union_no_duplicates l1 l2 =
  List.fold_left (fun acc x ->
    if List.mem x acc then acc else x :: acc
  ) l1 l2

(* Main unite function *)
let unite c lsts =
  let rec find_component acc remaining =
    let newly_connected, not_connected =
      List.partition (fun l -> List.exists (fun a -> connected c a l) acc) remaining
    in
    if newly_connected = [] then (acc, not_connected)
    else
      let merged = List.fold_left (fun a b -> b :: a) acc newly_connected in
      find_component merged not_connected
  in

  let rec aux remaining result =
    match remaining with
    | [] -> result
    | x :: xs ->
      let component, rest = find_component [x] xs in
      let merged =
        List.fold_left union_no_duplicates [] component
      in
      aux rest (merged :: result)
  in
  aux lsts []
