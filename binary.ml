
type tree = Node of int * tree * tree | Empty

let leaf _ = Empty

let node v l r = Node (v, l, r)




let inspect = function
  | Node (v, l, r) -> Some (v, l, r)
  | Empty -> None







let t1 = Node (8, Node (1, Empty, Node (6, Empty, Empty)),
  Node (12, Node (9, Empty, Empty), Node (42, Empty, Empty)))






let rec to_list = function
  | Empty -> []
  | Node (v, l, r) -> (to_list l) @ (v :: to_list r)
       







let rec insert x  = function
  | Empty -> Node (x, Empty, Empty)
  
  | Node (v, l, r) as t ->
    if x < v then
      Node (v, insert x l, r) 
    else if x > v then
      Node (v, l, insert x r)
    else
      t










let rec remove_max = function
  | Empty -> failwith "unreachable"
  | Node (v, l, Empty) -> v, l
  | Node (v, l, r) -> let v', r' = remove_max r in v', Node (v, l, r')

















let rec remove x = function
  | Empty -> Empty
  | Node (v, l, r) ->
    if x < v then
      Node (v, remove x l, r)
    else if x > v then
      Node (v, l, remove x r)
    else match l, r with
      | Empty, _ -> r
      | _, Empty -> l
      | _ -> let v', l' = remove_max l in Node (v', l', r)









