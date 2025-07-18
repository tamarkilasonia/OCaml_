

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)



let rec layer_tree r =
  let leaf = fun _ -> layer_tree (r + 1) in
  LNode (r, leaf, leaf)




let rec interval_tree l h =
  let m = (l +. h) /. 2. in
  let go lt rt _ = interval_tree lt rt in
  LNode ((l, h), go l m, go m h)




let rec rational_tree n d =
  let go l r _ = rational_tree l r in
  LNode ((n, d), go n (d + 1), go (n + 1) d)




let rec top n = function
  | _ when n = 0 -> Empty
  | LNode (v, l, r) ->
    let go t = top (n - 1) (t ()) in
    Node (v, go l, go r)




let rec map f (LNode (v, l, r)) =
  let go t _ = map f (t ()) in
  LNode (f v, go l, go r)




let find p t =
  let rec impl_find next_layer = function
  | [] -> impl_find [] next_layer
  | n :: layer -> match n () with
    | LNode (v, _, _) as t when p v -> t
    | LNode (_, l, r)               -> impl_find (l :: r :: next_layer) layer
  in impl_find [fun _ -> t] []
