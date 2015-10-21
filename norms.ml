(* Performs norms on lists *)

let rec kernel (x: float list) (y: float list) 
               (f: float -> float -> float) (z: float) : float =
    match (x,y) with
    | (h1::t1,h2::t2) -> kernel t1 t2 f (z +. (f h1 h2))
    | ([],[]) -> z
    | _ -> failwith "List error"

let one (x: float list) (y: float list) : float =
    kernel x y (fun x y -> abs_float (x -. y)) 0.

let two (x: float list) (y: float list) : float = 
    kernel x y (fun x y -> (x -. y) *. (x -. y)) 0.

;;

assert ( one [] [] = 0. );;
assert ( one [1.] [1.] = 0. );;
assert ( one [1.] [2.] = 1. );;
assert ( one [1.] [3.] = 2. );;
assert ( one [1.;2.;3.] [1.;2.;3.] = 0. );;
assert ( one [1.;2.;3.] [2.;2.;3.] = 1. );;
assert ( one [2.;2.;3.] [1.;2.;3.] = 1. );;
assert ( one [1.;2.;1.] [1.;2.;3.] = 2. );;
assert ( one [1.;2.;3.] [1.;2.;1.] = 2. );;
assert ( one [1.;2.;3.] [3.;2.;1.] = 4. );;

assert ( two [] [] = 0. );;
assert ( two [1.] [1.] = 0. );;
assert ( two [1.] [2.] = 1. );;
assert ( two [1.] [3.] = 4. );;
assert ( two [1.;2.;3.] [1.;2.;3.] = 0. );;
assert ( two [1.;2.;3.] [2.;2.;3.] = 1. );;
assert ( two [2.;2.;3.] [1.;2.;3.] = 1. );;
assert ( two [1.;2.;1.] [1.;2.;3.] = 4. );;
assert ( two [1.;2.;3.] [1.;2.;1.] = 4. );;
assert ( two [1.;2.;3.] [3.;2.;1.] = 8. );;
