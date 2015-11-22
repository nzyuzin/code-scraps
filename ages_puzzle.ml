(*
 * Let us solve the following puzzle: If you multiply my grand-son age by four,
 * you know how old I am. Now, if you exchange the two digits of our ages then
 * you have to multiply by three my age to get the age of my grand-son!
 *)

let exchange k =
  10 * (k mod 10) + (k / 10)

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_son_age * 4 = grand_father_age
  && exchange grand_father_age * 3 = exchange grand_son_age
  && (grand_father_age, grand_son_age) != (-1, -1)

let rec find answer =
  match answer with
  | (father, son) ->
      if son * 4 > father then
        (-1, -1)
      else if is_valid_answer answer then
        answer
      else
        let min_father = find (father - 1, son) in
        if is_valid_answer min_father then
          min_father
        else
          find (father, son + 1)

let () =
  let answer = find (99, 10) in
  match answer with
  | (f, s) ->
      print_endline ((string_of_int f) ^ "," ^ (string_of_int s))
