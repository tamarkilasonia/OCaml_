(*Consider the datatype ’a tree defined as:
 type ’a tree =
 | Leaf of ’a
 | Node of int * int * ’a tree * ’a tree
 The data in the tree are stored in the leaves, while each inner node, represented as Node(size,
 lsize, left, right), maintains two numbers along with the two subtrees left and right. The
 f
 irst number, size, denotes the total number of leaves in the tree, while the second one, lsize,
 records the number of leaves in the left subtree left. An example of such a well-formed tree is:
 let t = Node (3, 2,
 Node (2, 1, Leaf "a", Leaf "b"),
 Leaf "c")
 Provide implementations of the following functions:
 • update t i x — update t i x returns the tree obtained from t by replacing the contents of
 the ith leaf with x. Thus, for example:
 update t 2 "d" =Node (3, 2, Node (2, 1, Leaf "a", Leaf "b"), Leaf "d")
 • insert t i x — insert adds a new leaf at position i, shifting subsequent leaves right. More
 precisely, insert t i x returns the tree obtained from t by inserting a new subtree Leaf
 x at the ith position, while all leaves with original positions i,i + 1,... now have positions
 i +1,i+2,.... For example, insert t 1 "d") =
 Node (4, 3,
 Node (3, 1,
 Leaf "a",
 Node (2, 1, Leaf "d", Leaf "b")),
 Leaf "c")
 • list
 of t — list
 of t returns the list consisting of the elements stored in the leaves of t in
 left-to-right order. For example, list
 of t = ["a"; "b"; "c"]. Your function should run in
 time linear in the number of leaves of t. *)


  ’a tree-> int-> ’b-> ’b tree (5pts)
 let rec update t i x =
 match t with
 | Leaf v->
 if i = 0 then
 Leaf x
 else
 raise (Failure "Index out of bounds")
 | Node (size, lsize, left, right)-> if i < 0 || i >= size then
 t
 else if i < lsize then
 let new_left = update left i x in
 Node (size, lsize, new_left, right)
 else
 let new_right = update right (i- lsize) x in
 Node (size, lsize, left, new_right)


  insert = ’a tree-> int-> ’a-> ’a tree (8pts)
 let rec insert t i v =
 match t with
 | Leaf _->
 if i = 0 then Node (2, 1, Leaf v, t)
 else if i = 1 then Node (2, 1, t, Leaf v)
 else raise (Failure "index out of bounds")
 | Node (size, lsize, lt, rt)->
 if i < lsize then Node (size + 1, lsize + 1, insert lt i v, rt)
 else Node (size + 1, lsize, lt, insert rt (i- lsize) v)
 3. list
 of
 t = ’a tree-> ’a list (5pts)
 let rec list_of_t t =
 match t with
 | Leaf v->
 [v]
 | Node (_, _, left, right)->
 (list_of_t left) @ (list_of_t right)
 
