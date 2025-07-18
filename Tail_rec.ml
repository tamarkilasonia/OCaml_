(*N1*)
let rec fac n = if n < 2 then 1
            else n * fac (n-1)   

 (*N1-tail*)
 let fac2 n =
        let rec impl n acc=
        if n<2 then acc 
        else impl (n-1) (n*acc) 
in impl n 1

 (*N2*)
let rec remove a = function
 [] -> []
 | x::xs -> if x = a then remove a xs else x::remove a xs

(*N2-tail*)
let remove2 a l=
let rec impl acc l =
        match l with 
        | []-> acc
        | x::xs-> if x=a then impl xs acc (*თუ ლისტის პირველი ელემენტი უდრის a-ს, მას ვსკიპავთ და ვაგრძელებთ რეკურსიას დარჩენილ ელემენტებზე, ისე რომ acc არ იცვლება *)
        else impl xs (x::acc)  (*თუ ლისტის პირველი ელემენტი არ უდრის საძიებელ a-ს, მაშინ პირველ წევრს ვინახავთ acc -ში, რომელიც თავიდან არის []*)
in impl l []


let remove3 a l = List.rev (
        List.fold_left ( fun acc x -> if x=a then acc else x::acc) (*თუ x ტოლია a-ს (ანუ ის ელემენტია, რომელიც წასაშლელია), მაშინ არაფერს ვამატებთ acc-ში— უბრალოდ ვაბრუნებთ acc-ს.
 ხოლო თუ x არ უდრის a-ს, მაშინ x-ს ვამატებთ acc-ის თავში (x :: acc)*)
        [] l 
)


(*N3*)
let rec app l y = 
        match l with 
        |[]-> y
        |x::xs-> x :: app xs y  (* ორ ლისტს ვაერთებთ, მაგალითად [17] [4] -> [17,4]*)


(*N3-tail*)
let app l y =
  let rec impl acc l =
    match l with
    | [] -> List.rev_append acc y  (* Efficient reverse + append *)
    | x :: xs -> impl (x :: acc) xs
  in
  impl [] l



(*N4*)
let rec fibonacci n= if n<2 then 1 else fibonacci(n-1)+fibonacci(n-2)

(*N4-Tail*) 
let fib2 n =
        let rec impl n a b = 
                if n =0 then a 
                else impl (n-1) b (a+b)
        in
          impl n 0 1



let rec even n = 
  if n = 0 then "even" 
  else odd (n - 1)

and odd n = 
  if n = 0 then "odd" 
  else even (n - 1)




  let rec lenght = fun l -> match l with [] -> 0
  | x::xs-> 1 + lenght  xs

let length l =
  let rec impl l acc =
    match l with
    | [] -> acc
    | _::xs -> impl xs (acc + 1)
  in
  impl l 0

  (*impl [10; 20; 30] 0
→ _::xs = 10::[20;30], so xs = [20;30]
→ call impl [20;30] (0+1) = impl [20;30] 1

impl [20; 30] 1
→ _::xs = 20::[30], so xs = [30]
→ call impl [30] (1+1) = impl [30] 2

impl [30] 2
→ _::xs = 30::[], so xs = []
→ call impl [] (2+1) = impl [] 3

impl [] 3
→ match [] → return acc = 3
*)


let rec app a b =
        match a with 
        | []-> b 
        |x:xs-> x :: app xs b




let rec rev list = match list
 with []-> []
 | x::xs-> app (rev xs) [x]


 let rev list =
 let rec r2 a l =
 match l
 with []-> a
 | x::xs-> r2 (x::a) xs
 in r2 [] list





 let f = fun x -> x * x;;
map f [2; 3; 4];;

 let rec map f = function
 []->[]
 |x::xs-> f x :: map f xs


map f [2; 3; 4]
→ f 2 :: map f [3; 4]
→ 4 :: f 3 :: map f [4]
→ 4 :: 9 :: f 4 :: map f []
→ 4 :: 9 :: 16 :: []
→ [4; 9; 16]



