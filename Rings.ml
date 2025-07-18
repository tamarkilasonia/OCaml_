module type Ring = sig
  type t 
  val zero : t 
  val one : t 
  val to_string: t->string
  val compare: t->t-> int
  val add: t->t->t 
  val mul: t->t->t 
end
  (* Implement a module IntRing that implements the Ring signature
 for the int type.*)
module IntRing : Ring with type t= int=
struct
type t=int
let zero=0
let one=1
let to_string=string_of_int
let compare=Stdlib.compare
let add=(+)
let mul=( * )
end

(*Implement a module FloatRing that implements the Ring
 signature for the float type*)
module FloatRing : Ring with type t = float= struct 
type t=float 
let zero=0.
let one=1.
let to_string=string_of_float
let compare=Stdlib.compare
let add=(+.)
let mul=( *.)
end

(*Define a signature FiniteRing that extends the Ring signature
 with a value elems that represents a list of all elements of the
 ring's finite set. Make sure that everything in the Ring signature
 is part of FiniteRing (without copy-pasting them) *)
module type FiniteRing= sig 
include Ring
val elems : t list
end

module BoolRing : FiniteRing with type t = bool =
struct 
type t= bool
let zero= false
let one= true
let compare=Stdlib.compare
let to_string=string_of_bool
let add= (||)
let mul=(&&)
let elems=[false;true]
end 