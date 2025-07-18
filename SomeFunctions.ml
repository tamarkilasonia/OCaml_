(* Normal recursive factorial *)
let rec factorial n =
  if n = 0 then 1
  else n * factorial (n - 1)

(* Tail recursive factorial *)
let factorial_tail n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n - 1) (acc * n)
  in
  aux n 1


  (* Sum elements in a list using tail recursion *)
let sum_list lst =
  let rec aux acc lst =
    match lst with
    | [] -> acc
    | h :: t -> aux (acc + h) t
  in
  aux 0 lst
 
  (* Check whether an element exists in the list *)
let rec contains x lst =
  match lst with
  | [] -> false
  | h :: t -> h = x || contains x t



(* Tail recursive reverse function *)
let reverse lst =
  let rec aux acc l =
    match l with
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] lst


  (* Apply a function to every element in the list *)
let rec my_map f lst =
  match lst with
  | [] -> []
  | h :: t -> f h :: my_map f t


  (* Return true if string is palindrome *)
let is_palindrome s =
  let len = String.length s in
  let rec check i =
    if i >= len / 2 then true
    else if s.[i] <> s.[len - 1 - i] then false
    else check (i + 1)
  in
  check 0


  (* Compose two functions: f (g x) *)
let compose f g = fun x -> f (g x)

(* Example usage *)
let double x = 2 * x
let inc x = x + 1

let double_after_inc = compose double inc
(* double_after_inc 3 = double(4) = 8 *)


(* Dot product of two integer lists *)
let rec dot_product a b =
  match (a, b) with
  | ([], []) -> 0
  | (h1::t1, h2::t2) -> h1 * h2 + dot_product t1 t2
  | _ -> failwith "Lists must be of the same length"


  (* Find maximum element in a list *)
let max_list lst =
  let rec aux current_max lst =
    match lst with
    | [] -> current_max
    | h :: t -> aux (if h > current_max then h else current_max) t
  in
  match lst with
  | [] -> failwith "Empty list"
  | h :: t -> aux h t


  (* Combine two lists into pairs *)
let rec zip l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | (h1::t1, h2::t2) -> (h1, h2) :: zip t1 t2
  | _ -> failwith "Lists must be of equal length"


  (* Remove all elements equal to x *)
let rec remove_all x lst =
  match lst with
  | [] -> []
  | h :: t -> if h = x then remove_all x t else h :: remove_all x t

  (* Transpose a matrix (list of lists) *)
let transpose matrix =
  let rec aux m acc =
    match m with
    | [] | [] :: _ -> List.rev acc
    | _ -> aux (List.map List.tl m) ((List.map List.hd m) :: acc)
  in
  aux matrix []


  (* Count how many times each element appears *)
let count_occurrences lst =
  let tbl = Hashtbl.create 10 in
  List.iter (fun x ->
    let count = try Hashtbl.find tbl x with Not_found -> 0 in
    Hashtbl.replace tbl x (count + 1)
  ) lst;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []


  (* Generate a random list of integers *)
let random_list n max_val =
  let rec aux i acc =
    if i = 0 then acc
    else aux (i - 1) (Random.int max_val :: acc)
  in
  aux n []


  (* Greatest Common Divisor *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)


  (* Flatten a list of lists into a single list *)
let rec flatten lst =
  match lst with
  | [] -> []
  | h :: t -> h @ flatten t



  (* Take the first n elements from a list *)
let rec take n lst =
  match (n, lst) with
  | (0, _) | (_, []) -> []
  | (n, h :: t) -> h :: take (n - 1) t
