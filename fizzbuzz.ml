let fizzbuzz =
  let rec inner n =
    match n with
    | 100 ->
        print_int 100;
        print_endline "";
        100
    | newN ->
        if newN mod 3 = 0 then print_string "Fizz";
        if newN mod 5 = 0 then print_string "Buzz";
        if newN mod 3 > 0 && newN mod 5 > 0 then print_int newN;
        print_endline "";
        inner (n + 1) in
  inner 1

