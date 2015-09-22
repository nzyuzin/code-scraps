let pow (x : float) (n : int) : float =
  let rec pow' (var_x : float) (var_n : int) : float =
    if var_n = 0 then 1.
    else pow' (var_x * x) (var_n - 1) in
  pow' x n

let math_pi = 4. *. atan(1.)
let m = 1.
let l = 1.
let g = 9.8

let phi pi = pi /. m

let pi phi = -.m *. g *. sin(phi)

let energy pi phi = pow (pi, 2.)  /. (2. *. m) + m *. l * g * (1. -. cos phi);;

print_float(pi(math_pi));;
print_endline "";;

print_float(energy(math_pi, 1.));;
print_endline "";;

