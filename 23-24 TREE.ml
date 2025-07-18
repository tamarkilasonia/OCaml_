
(* Definition of the tree type *)
type 'a tree =
  | Leaf of 'a
  | One of int * 'a tree
  | Two of int * int * 'a tree * 'a tree

(* Search for the i-th leaf in the tree *)
let rec search t i =
  match t with
  | Leaf x ->
      if i = 0 then Some x else None
  | One(size, t1) ->
      if i < size then search t1 i else None
  | Two(lsize, rsize, left, right) ->
      if i < lsize then search left i
      else if i < lsize + rsize then search right (i - lsize)
      else None


(* Update the i-th leaf in the tree with a new value *)
let rec update t i x =
  match t with
  | Leaf _ ->
      if i = 0 then (true, Leaf x)
      else (false, t)
  | One(size, t1) ->
      if i < size then
        let (ok, newt1) = update t1 i x in
        (ok, One(size, newt1))
      else (false, t)
  | Two(lsize, rsize, left, right) ->
      if i < lsize then
        let (ok, newleft) = update left i x in
        (ok, Two(lsize, rsize, newleft, right))
      else if i < lsize + rsize then
        let (ok, newright) = update right (i - lsize) x in
        (ok, Two(lsize, rsize, left, newright))
      else (false, t)

(* Insert a new leaf at position i in the tree *)
let rec insert t i x =
  match t with
  | Leaf y ->
      if i = 0 then Two(1, 1, Leaf x, Leaf y)
      else if i = 1 then Two(1, 1, Leaf y, Leaf x)
      else failwith "index out of bounds"
  | One(size, t1) ->
      if i <= size then
        let newt = insert t1 i x in
        One(size + 1, newt)
      else failwith "index out of bounds"
  | Two(lsize, rsize, left, right) ->
      if i <= lsize then
        let newleft = insert left i x in
        Two(lsize + 1, rsize, newleft, right)
      else if i <= lsize + rsize then
        let newright = insert right (i - lsize) x in
        Two(lsize, rsize + 1, left, newright)
      else failwith "index out of bounds"

(* Remove the i-th leaf from the tree and return the value and new tree *)
let rec remove t i =
  match t with
  | Leaf x ->
      if i = 0 then (x, None)
      else failwith "index out of bounds"
  | One(size, t1) ->
      if i < size then
        let (x, opt) = remove t1 i in
        let newtree = match opt with
          | Some t' -> Some (One(size - 1, t'))
          | None -> None
        in
        (x, newtree)
      else failwith "index out of bounds"
  | Two(lsize, rsize, left, right) ->
      if i < lsize then
        let (x, opt) = remove left i in
        let newtree = match opt with
          | Some t' -> Some (Two(lsize - 1, rsize, t', right))
          | None -> Some (One(rsize, right))
        in
        (x, newtree)
      else if i < lsize + rsize then
        let (x, opt) = remove right (i - lsize) in
        let newtree = match opt with
          | Some t' -> Some (Two(lsize, rsize - 1, left, t'))
          | None -> Some (One(lsize, left))
        in
        (x, newtree)
      else failwith "index out of bounds"
