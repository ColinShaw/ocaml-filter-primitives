(* Operations on complex numbers *)

type complex = { re: float; im: float }


let add (x: complex) (y: complex) : complex =
    { re = (x.re +. y.re); im = (x.im +. y.im) } 

let subtract (x: complex) (y: complex) : complex =
    { re = (x.re -. y.re); im = (x.im -. y.im) }

let multiply (x: complex) (y: complex) : complex =
    let r = (x.re *. y.re) -. (x.im *. y.im) 
    and i = (x.im *. y.re) +. (x.re *. y.im)
    in { re = r ; im = i }

let magnitude (x: complex) : float =
    sqrt ( (x.re *. x.re) +. (x.im *. x.im) )

let angle (x: complex) : float =
    atan ( x.im /. x.re )

let from_cartesian (x: float * float) : complex =
    let (r,i) = x in { re = r; im = i }

let to_cartesian (x: complex) : float * float =
    ( x.re , x.im )

let from_radial (x: float * float) : complex =
    let (m,a) = x in
    let r = m *. cos a
    and i = m *. sin a
    in { re = r ; im = i }

let to_radial (x: complex) : float * float =
    ( magnitude x , angle x )

;;

assert ( add {re=0.;im=0.} {re=0.;im=0.} = {re=0.;im=0.} );;
assert ( add {re=1.;im=1.} {re=1.;im=1.} = {re=2.;im=2.} );;
assert ( add {re=1.;im=1.} {re=1.;im=(-1.)} = {re=2.;im=0.} );;

assert ( subtract {re=0.;im=0.} {re=0.;im=0.} = {re=0.;im=0.} );;
assert ( subtract {re=1.;im=1.} {re=1.;im=1.} = {re=0.;im=0.} );;
assert ( subtract {re=1.;im=1.} {re=1.;im=(-1.)} = {re=0.;im=2.} );;

assert ( multiply {re=0.;im=0.} {re=0.;im=0.} = {re=0.;im=0.} );;
assert ( multiply {re=1.;im=0.} {re=0.;im=0.} = {re=0.;im=0.} );;
assert ( multiply {re=1.;im=0.} {re=1.;im=0.} = {re=1.;im=0.} );;
assert ( multiply {re=0.;im=1.} {re=0.;im=0.} = {re=0.;im=0.} );;
assert ( multiply {re=0.;im=1.} {re=0.;im=1.} = {re=(-1.);im=0.} );;
assert ( multiply {re=1.;im=1.} {re=1.;im=1.} = {re=0.;im=2.} );;
assert ( multiply {re=1.;im=(-1.)} {re=1.;im=(-1.)} = {re=0.;im=(-2.)} );;

assert ( magnitude {re=0.;im=0.} = 0. );;
assert ( magnitude {re=0.;im=1.} = 1. );;
assert ( magnitude {re=1.;im=0.} = 1. );;
assert ( magnitude {re=1.;im=1.} = (sqrt 2.) );;

assert ( angle {re=1.;im=0.} = 0. );;
assert ( angle {re=1.;im=1.} = (atan 1.) );;
assert ( angle {re=1.;im=(-1.)} = (atan (-1.)) );;
assert ( angle {re=(-1.);im=1.} = (atan (-1.)) );;

assert ( from_cartesian (0.,0.) = {re=0.;im=0.} );;
assert ( from_cartesian (0.,1.) = {re=0.;im=1.} );;
assert ( from_cartesian (1.,0.) = {re=1.;im=0.} );;

assert ( to_cartesian {re=0.;im=0.} = (0.,0.) );;
assert ( to_cartesian {re=0.;im=1.} = (0.,1.) );;
assert ( to_cartesian {re=1.;im=0.} = (1.,0.) );;

assert ( from_radial (0.,0.) = {re=0.;im=0.} );;
assert ( from_radial (0.,1.) = {re=0.;im=0.} );;
assert ( from_radial (1.,0.) = {re=1.;im=0.} );;
assert ( from_radial (1.,(asin 1.)) = {re=0.;im=1.} );;

assert ( to_radial {re=0.;im=0.} = (0.,0.) );;
assert ( to_radial {re=1.;im=0.} = (0.,0.) );;
assert ( to_radial {re=0.;im=1.} = (0.,0.) );;
assert ( to_radial {re=1.;im=1.} = (0.,0.) );;
