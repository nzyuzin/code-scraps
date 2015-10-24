(* This program prints a lattice of size N on screen *)

let check_usage =
  if Array.length Sys.argv != 2 then begin
    let program_name : string = Sys.argv.(0) in
    print_endline ("Usage: " ^ program_name ^ " N");
    exit 1
  end

let () =
  check_usage;
  let edges = ref 0 in
  let print_edge chr =
    edges := !edges + 1;
    print_char chr in

  let n = int_of_string Sys.argv.(1) in
  for i = 0 to n * 2 do
    if i mod 2 = 0 then
      for j = 0 to n * 2 do
        if j mod 2 = 1 then
          print_edge '-'
        else
          print_char ' '
      done
    else
      for j = 0 to n * 2 do
        if j mod 2 = 0 then
          print_edge '|'
        else
          print_char ' '
      done;
    print_char '\n'
  done;
  print_endline (string_of_int !edges)
