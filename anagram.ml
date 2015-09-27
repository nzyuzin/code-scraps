(* This program computes random permutation of characters in a given word *)

let anagram (word : string) : string =
  let random_char (arr : string) : char =
    let n = Random.int (String.length arr) in
    String.get arr n in

  let del_char (str : string) (chr : char) : string =
    let char_pos : int = String.index str chr in
    let before_chr : string = String.sub str 0 char_pos in
    let after_chr_len : int = (String.length str) - char_pos - 1 in
    let after_chr : string = String.sub str (char_pos + 1) after_chr_len  in
    before_chr ^ after_chr in

  let rec inner (current_word : string) (remainder : string) : string =
    if remainder = "" then
      current_word
    else
      let chosen_char : char = random_char remainder in
      let word_plus_char : string = current_word ^ (String.make 1 chosen_char) in
      let remainder_without_char : string = del_char remainder chosen_char in
      inner word_plus_char remainder_without_char in
  inner "" word

let word = Sys.argv.(1)
let lower_word = String.lowercase word

let _ = Random.self_init ();;

print_endline (anagram lower_word)
