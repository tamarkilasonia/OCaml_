(* === Module type for Elements === *)
module type Elements = sig
  type t
  val order : t -> t list
  val is_valid : t -> bool
  val addition : t -> t -> t
end

(* === Module type for Set === *)
module type Set = sig
  type t
  type elt

  val order : t -> t
  val is_valid : t -> bool
  val is_empty : t -> bool
  val is_member : elt -> t -> bool
  val are_equal : t -> t -> bool
  val is_subset : t -> t -> bool
  val insert : elt -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersection : t -> t -> t
  val addition : t -> t -> t
end

(* === Sample Module Integers === *)
module Integers : Elements with type t = int = struct
  type t = int
  let order x = [x]
  let is_valid _ = true
  let addition x y = x + y
end

(* === Sample Module Strings === *)
module Strings : Elements with type t = string = struct
  type t = string
  let order x = [x]
  let is_valid _ = true
  let addition x y = x ^ y
end

(* === Sample Module IntegerLists === *)
module IntegerLists : Elements with type t = int list = struct
  type t = int list
  let order x = List.sort compare x
  let is_valid lst =
    let rec no_duplicates l =
      match l with
      | [] -> true
      | x::xs -> not (List.mem x xs) && no_duplicates xs
    in no_duplicates lst
  let addition x y = x @ y
end


(* === Functor Set_of === *)
module Set_of (E : Elements) : (Set with type elt = E.t and type t = E.t list) = struct
  type elt = E.t
  type t = elt list

  let is_valid c =
    let rec no_duplicates lst =
      match lst with
      | [] -> true
      | x::xs -> not (List.mem x xs) && no_duplicates xs
    in
    List.for_all E.is_valid c && no_duplicates c

  let order c =
    if is_valid c then List.sort compare c
    else failwith "order: invalid collection"

  let is_empty c = c = []

  let is_member x c = List.exists (fun y -> x = y) c

  let insert x c =
    if not (E.is_valid x) then failwith "Invalid element"
    else if is_member x c then c else x :: c

  let remove x c = List.filter (fun y -> y <> x) c

  let is_subset a b = List.for_all (fun x -> is_member x b) a

  let are_equal a b = is_subset a b && is_subset b a

  let union a b = List.fold_left (fun acc x -> insert x acc) a b

  let intersection a b = List.filter (fun x -> is_member x b) a

  let addition a b =
    if is_valid a && is_valid b then E.addition a b
    else failwith "addition: invalid sets"
end
