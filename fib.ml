let fib n =
  let rec fib_inner a b m =
    match m with
    | 0 -> a
    | _ -> fib_inner b (a + b) (m - 1) in
  fib_inner 0 1 n

