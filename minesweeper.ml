type cell =
  | Number of int
  | Empty
  | Mine

type grid = cell array array

exception Boom of string

let new_grid (width : int) (height : int) : grid =
  Array.make_matrix width height Empty

let grid_mapi (g : grid) (change_cell : int -> int -> cell) : grid =
  let change_col idx idy _ =
    change_cell idx idy in
  let change_row idx _ =
    Array.mapi (change_col idx) g.(idx) in
  Array.mapi change_row g

let is_ingrid (g : grid) (x : int) (y : int) : bool =
  let width = Array.length g in
  let height = Array.length g.(0) in
  (x >= 0) && (x < width) && (y >= 0) && (y < height)

let fill_cell (g : grid) (x : int) (y : int) (content : cell) : grid =
  let change_cell i j =
    if i = x && j = y then
      content
    else
      g.(i).(j) in
  grid_mapi g change_cell

let has_mine (g : grid) x y : bool =
  g.(x).(y) = Mine

let place_mines (g : grid) (number : int) : grid =
  let width = Array.length g in
  let height = Array.length g.(0) in
  let rec inner g' iter =
    if iter > number then
      g'
    else
      let x = Random.int width in
      let y = Random.int height in
      inner (fill_cell g' x y Mine) (iter + 1) in
  inner g 0

let count_mines (g : grid) x y =
  let rec vloop dx =
    let rec hloop dy =
      let i = x + dx in
      let j = y + dy in
      let value =
        if is_ingrid g i j then
          match g.(i).(j) with
          | Mine -> 1
          | _ -> 0
    else
      0 in
      if dy = 1 then
        value
        else
          hloop (dy + 1) + value in
    if dx = 1 then
      hloop (-1)
      else
        vloop (dx + 1) + hloop (-1) in
  vloop (-1)

let place_numbers (g : grid) : grid =
  let change_cell i j =
    match g.(i).(j) with
    | Empty ->
        let mines = count_mines g i j in
        if mines != 0 then Number mines else Empty
    | x -> x in
  grid_mapi g change_cell

let print_grid (g : grid) (sl : (int * int) list) (fl : (int * int) list) endgame =
  let width = Array.length g in
  let height = Array.length g.(0) in
  let print_row first_char second_char =
      for j = 0 to height * 2 do
        if j mod 2 = 0 then
          print_char first_char
        else
          print_char (second_char (j/2))
      done in
  let extract_char i j =
    let list_contains l el =
      List.exists (fun x -> x = el) l in
    if list_contains fl (i,j) then
      'F'
    else if (not endgame) && not (list_contains sl (i,j)) then
      '?'
    else
    match g.(i).(j) with
    | Empty -> ' '
    | Number x -> char_of_int (x + 48) (* ASCII conversion *)
    | Mine -> 'X' in
  for i = 0 to width * 2 do
    if i mod 2 = 0 then
      print_row ' ' (fun x -> '-')
    else
      print_row '|' (extract_char (i/2));
    print_char '\n'
  done

let new_minefield ((w, h) : int * int) (mines : int) : grid =
  let built_grid = place_mines (new_grid w h) mines in
  place_numbers (built_grid)

let gameloop grid =
  let rec inner sl fl =
    print_grid grid sl fl false;
    print_endline ";; Enter your move:";
    print_string "-->  ";
    let move = read_line () in
    let op = String.get move 0 in
    let x = int_of_char (String.get move 2) - 48 in (* ASCII Conversion *)
    let y = int_of_char (String.get move 4) - 48 in
    if op = 's' && has_mine grid x y then begin
      print_grid grid [] fl true;
      raise (Boom "You lost.")
    end
    else
      match op with
      | 's' ->
          inner ((x, y) :: sl) fl
      | 'f' ->
          inner sl ((x, y) :: fl)
      | _ ->
          print_endline "Unrecognized operation: only 's' (step) and 'f' (flag) are supported";
          inner sl fl in
  inner [] []

let () =
  Random.self_init ();
  gameloop (new_minefield (9, 9) 10);;
