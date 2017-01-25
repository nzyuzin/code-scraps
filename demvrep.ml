let verbose = ref false
let graphviz = ref false

type alignment =
  | Rep
  | Dem

type node = int * alignment
type edge = int * int

type 'a set = 'a list

type graph = node set * edge set

let nodes (g : graph): node set =
  fst g

let edges (g : graph): edge set =
  snd g

let node_id (n: node): int = fst n

let node_alignment (n: node): alignment = snd n

let string_of_alignment = function
  | Rep -> "Rep"
  | Dem -> "Dem"

let string_of_node (n : node) = "(" ^ (string_of_int (fst n))
  ^ ", " ^ (string_of_alignment (node_alignment n)) ^ ")"

let string_of_edge (e : edge) = "(" ^ (string_of_int (fst e))
  ^ ", " ^ (string_of_int (snd e)) ^ ")"

let string_of_set (to_str : 'a -> string) (l : 'a set): string =
  let rec inner = function
    | [] -> "}"
    | a :: [] -> (to_str a) ^ "}"
    | a :: rest -> ((to_str a) ^ ", ") ^ (inner rest) in
  "{" ^ (inner l)

let print_graph g =
  let cmp_n (f: node) (s: node) = (fst f) - (fst s) in
  let cmp_e (f: edge) (s: edge) =
    let fdiff = (fst f) - (fst s) in
    if fdiff != 0 then fdiff
    else (snd f) - (snd s) in
  let nds = string_of_set string_of_node (List.sort cmp_n (nodes g)) in
  let edgs = string_of_set string_of_edge (List.sort cmp_e (edges g)) in
  print_endline ("<" ^ nds ^ " : " ^ edgs ^ ">")

let unwrap_optional = function
  | Some x -> x
  | None -> raise (Failure "unwrap optional failed!")

let lookup id g =
  let rec inner = function
    | [] -> None
    | nd :: rest ->
        if (node_id nd) = id then Some nd
        else inner rest in
  inner (nodes g)

let same_node f s: bool =
  (node_id f) = (node_id s)

let same_alignment (f: node) (s: node): bool =
  (node_alignment f) = (node_alignment s)

let same_edge f s: bool =
  (fst f) = (fst s) && (snd f) = (snd s)

let make_node (id: int) (algnmt: alignment): node =
  (id, algnmt)

let id_counter = ref 0

let new_node algnmt: node = begin
  let nd = make_node !id_counter algnmt in
  id_counter := !id_counter + 1;
  nd
end

let make_edge f s: edge =
  if f < s then
    (f, s)
  else
    (s, f)

let make_graph (nds: node set) (edgs: edge set): graph =
  (nds, edgs)

let contains_node g n: bool =
  let rec inner = function
    | [] -> false
    | a :: rest -> if same_node n a then true else inner rest in
  inner (nodes g)

let contains_edge g e: bool =
  let rec inner = function
    | [] -> false
    | a :: rest -> if same_edge e a then true else inner rest in
  inner (edges g)

let neighbour (e: edge) (n: int) =
  if (fst e) = n then
    Some (snd e)
  else if (snd e) = n then
    Some (fst e)
  else
    None

let add_node g n: graph =
  if contains_node g n then g
  else make_graph (n :: (nodes g)) (edges g)

let add_edge (g: graph) (e: edge): graph =
  if contains_edge g e then g
  else make_graph (nodes g) (e :: (edges g))

let neighbours (n: node) (g: graph) : node set =
  let rec inner = function
    | [] -> []
    | e :: rest -> begin
      match neighbour e (node_id n) with
      | Some other_node_id -> (unwrap_optional (lookup other_node_id g)) :: (inner rest)
      | None -> inner rest
    end in
  inner (edges g)

let reps (nds: node set): int =
  List.length (List.filter (fun x -> Rep = (node_alignment x)) nds)

let dems (nds: node set): int =
  List.length (List.filter (fun x -> Dem = (node_alignment x)) nds)

let change_alignment (n: node) (algnmt: alignment): node =
  make_node (node_id n) algnmt

let simulate_turn (g: graph): graph =
  let rec inner old_g_nodes (new_g: graph) =
    match old_g_nodes with
    | [] -> new_g
    | n :: rest ->
      let neighbrs = neighbours n g in
      let self_algnmt = begin
        match (node_alignment n) with
        | Rep -> 1
        | Dem -> -1
      end in
      if (reps neighbrs) + self_algnmt > (dems neighbrs) then
        inner rest (add_node new_g (change_alignment n Rep))
      else if (reps neighbrs) + self_algnmt < (dems neighbrs) then
        inner rest (add_node new_g (change_alignment n Dem))
      else
        inner rest (add_node new_g n) in
  let new_graph = make_graph [] (edges g) in
  inner (nodes g) new_graph

let is_stable f s t =
  let rec inner = function
    | [] -> true
    | fn :: rest ->
      let sn = unwrap_optional (lookup (node_id fn) s) in
      let tn = unwrap_optional (lookup (node_id fn) t) in
      let node_stable = ((same_alignment fn sn) && (same_alignment fn tn))
        || ((not (same_alignment fn sn)) && (same_alignment fn tn)) in
      if node_stable then inner rest
      else false in
  inner (nodes f)

let rec iterate_until_stable g break: int * graph =
  let first_execution = ref g in
  let second_execution = ref (simulate_turn !first_execution) in
  if is_stable !first_execution !first_execution !second_execution then
    (1, !second_execution)
  else
    let third_execution = ref (simulate_turn !second_execution) in
    let rec inner iterations =
      if is_stable !first_execution !second_execution !third_execution then
        (iterations, !third_execution)
      else if iterations >= break then
        (-1, make_graph [] [])
      else begin
        if !verbose then begin
          print_endline ("Intermediate graph on iteration " ^ (string_of_int iterations) ^ ":");
          print_graph !second_execution
        end;
        first_execution := !second_execution;
        second_execution := !third_execution;
        third_execution := (simulate_turn !second_execution);
        inner (iterations + 1)
      end in
  inner 2

let read_graph () =
  let parse_node str: node =
    let fail () = raise (Failure ("Unexpected input instead of node: " ^ str)) in
    let letter = String.sub str 0 1 in
    if String.length str >= 1 then
      if letter = "r" then new_node Rep
      else if letter = "d" then new_node Dem
      else fail ()
    else fail () in
  let parse_edge str: edge =
    let splt = String.split_on_char ' ' str in
    if List.length splt == 2 then
      let f = int_of_string (List.hd splt) in
      let s = int_of_string (List.nth splt 1) in
      make_edge f s
    else
      raise (Failure ("Unexpected input instead of edge: " ^ str)) in
  let rec inner (g: graph) nodes_read =
    let line = read_line () in
    if (String.length line) = 0 then
      if nodes_read then
        g
      else
        inner g true
    else
      if nodes_read then
        let edg = parse_edge line in
        inner (add_edge g edg) nodes_read
      else
        let nd = parse_node line in
        inner (add_node g nd) nodes_read in
  inner (make_graph [] []) false

let break = ref 1000

let print_to_graphviz (g: graph) (name: string): unit =
  let print_edges n =
    let rec inner = function
      | [] -> print_endline "}"
      | nghbr :: rest -> begin
          print_string ((string_of_int (node_id nghbr)) ^ " ");
          inner rest
        end in
    inner (neighbours n g) in
  let rec print_nodes = function
    | [] -> ()
    | n :: rest -> begin
        print_string ((string_of_int (node_id n)) ^ " -> {");
        print_edges n;
        print_nodes rest
      end in
  let color = function
    | Rep -> "red"
    | Dem -> "blue" in
  let rec print_colors = function
    | [] -> ()
    | n :: rest -> print_endline ((string_of_int (node_id n))
          ^ " [color=" ^ (color (node_alignment n)) ^ ",style=filled];");
        print_colors rest in
  begin
    print_endline ("digraph " ^ name ^ " {");
    print_nodes (nodes g);
    print_colors (nodes g);
    print_endline "}"
  end

let print_only_final = ref false
let print_only_initial = ref false

let command_line_arguments = [
  ("-b", Arg.Set_int(break), "<number> Turns until ceasing attempts to stabilize. Any value less than 2 is ignored. Default is 1000");
  ("-v", Arg.Set(verbose), "Be verbose about simulation steps");
  ("-g", Arg.Set(graphviz), "Print resulting output in graphviz understood format");
  ("-f", Arg.Set(print_only_final), "Print only final graph");
  ("-i", Arg.Set(print_only_initial), "Print only initial graph");
]

let _ =
  begin
    Arg.parse command_line_arguments (fun x -> ()) ("Usage: " ^ Sys.argv.(0) ^ " [-b]");
    let initial_graph = read_graph () in
    let (time, final_graph) = iterate_until_stable initial_graph !break in
    if time != -1 then
      begin
        if not (!print_only_initial || !print_only_final) then begin
          print_string "Turns to stabilize: ";
          print_int time;
          print_endline "";
          print_endline "Initial graph: ";
        end;
        if (not !print_only_final) then
          if !graphviz then
            print_to_graphviz initial_graph "INITIAL"
          else
            print_graph initial_graph;
        if not (!print_only_initial || !print_only_final) then print_endline "Resulting graph: ";
        if (not !print_only_initial) then
          if !graphviz then
            print_to_graphviz final_graph "FINAL"
          else
            print_graph final_graph;
      end
    else
      prerr_endline ("Couldn't stabilize in " ^ (string_of_int !break) ^ " turns!")
  end

