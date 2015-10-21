let pi = 4. *. atan 1.

let sinc_primitive (x: float) : float = 
    match x with 
    | 0. -> 1.
    | _ -> (sin x) /. x

let sinc (i: int) (m: int) (fc: float) : float =
    let m' = float_of_int m in
    let i' = float_of_int i in
    let arg1 = 2. *. pi *. fc in
    let arg2 = i' -. (m' /. 2.) in
    if (i - m / 2) = 0 then arg1
    else (sin (arg1 *. arg2)) /. arg1  

let blackmann (i: int) (m: int) (fc: float) : float =
    let m' = float_of_int m in
    let i' = float_of_int i in
    let arg = 2. *. pi *. i' /. m' in
    0.42 -. 0.5 *. cos (arg) +. 0.08 *. cos (2. *. arg)

let hamming (i: int) (m: int) (fc: float) : float =
    let m' = float_of_int m in
    let i' = float_of_int i in
    let arg = 2. *. pi *. i' /. m' in
    0.54 -. 0.46 *. cos (arg) 


let gen (m: int) (fc: float) (f: int -> int -> float -> float) : float list =
    let rec g (m: int) (fc: float) (f: int -> int -> float -> float) (i: int) : float list =
        if i = 0 then [f i m fc]
        else (f i m fc)::(g m fc f (i-1))
    in g m fc f m

let gen_sinc (m: int) (fc: float) : float list =
    gen m fc sinc
    
let gen_sinc_blackmann (m: int) (fc: float) : float list =
    gen m fc (fun x y z -> sinc x y z *. blackmann x y z)

let gen sinc_hamming (m: int) (fc: float) : float list =
    gen m fc (fun x y z -> sinc x y z *. hamming x y z)



