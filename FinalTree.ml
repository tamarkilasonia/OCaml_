type 'a tree = 
    Leaf of 'a
  | One of int * 'a tree
  | Two of int * int * 'a tree * 'a tree;;

let rec search t i = if i < 0 then None else match t with
    | Leaf a               -> if i == 0     then Some a else None
    | One (n, t1)          -> if i >= n     then None   else search t1 i
    | Two (n1, n2, t1, t2) -> if i >= n1+n2 then None
        else if i >= n1 then search t2 (i - n1)
        else search t1 i;;

let rec update t i x = if i < 0 then (false, t) else match t with
    | Leaf a               -> if i == 0     then (true, Leaf x) else (false, t)
    | One (n, t1)          -> if i >= n     then (false, t)     else (match update t1 i x with
        | (b, new_t1) -> (b, One (n, new_t1)))
    | Two (n1, n2, t1, t2) -> if i >= n1+n2 then (false, t)     else
        if i >= n1 then match update t2 (i - n1) x with
          | (b, new_t2) -> (b, Two (n1, n2, t1, new_t2))
        else            match update t1 i x with
          | (b, new_t1) -> (b, Two (n1, n2, new_t1, t2));;

let rec insert t i v = if i < 0 then raise (Failure "index out of bounds") else match t with
    | Leaf a               -> if i == 0 then Two (1, 1, Leaf v, Leaf a) 
        else if i == 1 then Two (1, 1, Leaf v, Leaf a) 
        else raise (Failure "index out of bounds")
    | One (n, t1)          -> if i > n     then raise (Failure "index out of bounds") else One (n, insert t1 i v)
    | Two (n1, n2, t1, t2) -> if i > n1+n2 then raise (Failure "index out of bounds")
        else if i >= n1 then Two (n1, n2+1, t1, insert t2 (i-n1) v)
        else                 Two (n1+1, n2, insert t1 i v, t2);;

let rec remove tr i = if i < 0 then raise (Failure "index out of bounds") else match tr with
    | Leaf a               -> if i == 0     then (a, None) else raise (Failure "index out of bounds")
    | One (n, t1)          -> if i >= n     then raise (Failure "index out of bounds")
        else let (a, b) = remove t1 i in (match b with
            | None        -> (a, None)
            | Some new_t1 -> (a, Some (One (n-1, new_t1))))
    | Two (n1, n2, t1, t2) -> if i >= n1+n2 then raise (Failure "index out of bounds")
        else if i >= n1 then let (a, b) = remove t2 (i-n1) in (match b with
            | None        -> (a, Some (One (n1, t1)))
            | Some new_t2 -> (a, Some (Two (n1, n2-1, t1, new_t2))))
        else let (a, b) = remove t1 i in (match b with
            | None        -> (a, Some (One (n2, t2)))
            | Some new_t1 -> (a, Some (Two (n1-1, n2, new_t1, t2))));;
