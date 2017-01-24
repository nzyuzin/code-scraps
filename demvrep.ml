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

let string_of_alignment = function
  | Rep -> "Rep"
  | Dem -> "Dem"

let string_of_node (n : node) = "(" ^ (string_of_int (fst n))
  ^ ", " ^ (string_of_alignment (snd n)) ^ ")"

let string_of_edge (e : edge) = "(" ^ (string_of_int (fst e))
  ^ ", " ^ (string_of_int (snd e)) ^ ")"

let string_of_list (to_str : 'a -> string) (l : 'a list): string =
  let rec inner = function
    | [] -> "]"
    | a :: [] -> (to_str a) ^ "]"
    | a :: rest -> ((to_str a) ^ ", ") ^ (inner rest) in
  "[" ^ (inner l)

let print_graph g =
  let cmp_n (f: node) (s: node) = (fst f) - (fst s) in
  let cmp_e (f: edge) (s: edge) =
    let fdiff = (fst f) - (fst s) in
    if fdiff != 0 then fdiff
    else (snd f) - (snd s) in
  let nds = string_of_list string_of_node (List.sort cmp_n (nodes g)) in
  let edgs = string_of_list string_of_edge (List.sort cmp_e (edges g)) in
  print_endline ("<" ^ nds ^ " : " ^ edgs ^ ">")

let lookup id g =
  let rec inner = function
    | [] -> None
    | nd :: rest ->
        if (fst nd) = id then Some nd
        else inner rest in
  inner (nodes g)

let same_node f s: bool =
  (fst f) = (fst s)

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

let make_edge fn sn: edge =
  let f = fst fn in
  let s = fst sn in
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

let neighbour e n =
  if (fst e) = n then
    Some (snd e)
  else if (snd e) = n then
    Some (fst e)
  else
    None

let add_node g n: unit =
  if contains_node !g n then
    ()
  else
    g := make_graph (n :: (nodes !g)) (edges !g)

let add_edge g e: unit =
  if contains_edge !g e then
    ()
  else
    g := make_graph (nodes !g) (e :: (edges !g))

let neighbours (n: node) (g: graph) : node set =
  let unwrap_optional = function
    | Some x -> x
    | None -> raise (Failure "unwrap optional failed!") in
  let rec inner = function
    | [] -> []
    | e :: rest -> begin
      match neighbour e (fst n) with
      | Some other_node_id -> (unwrap_optional (lookup other_node_id g)) :: (inner rest)
      | None -> inner rest
    end in
  inner (edges g)

let reps (nds: node set): int =
  List.length (List.filter (fun x -> Rep = (snd x)) nds)

let dems (nds: node set): int =
  List.length (List.filter (fun x -> Dem = (snd x)) nds)

let change_alignment (n: node) (algnmt: alignment): node =
  make_node (fst n) algnmt

let simulate_turn (g: graph): graph =
  let rec inner old_g_nodes (new_g : graph ref) =
    match old_g_nodes with
    | [] -> new_g
    | n :: rest ->
      let neighbrs = neighbours n g in
      if (reps neighbrs) > (dems neighbrs) then begin
        add_node new_g (change_alignment n Rep);
        inner rest new_g
      end
      else begin
        add_node new_g (change_alignment n Dem);
        inner rest new_g
      end in
  let new_graph = make_graph [] (edges g) in
  !(inner (nodes g) (ref new_graph))

let _ =
  begin
    let initial_graph = ref (make_graph [] []) in
    let fst_node = new_node Dem in
    let snd_node = new_node Rep in
    let thrd_node = new_node Rep in
    add_node initial_graph fst_node;
    add_node initial_graph snd_node;
    add_node initial_graph thrd_node;
    add_edge initial_graph (make_edge fst_node snd_node);
    add_edge initial_graph (make_edge fst_node thrd_node);
    print_graph !initial_graph;
    print_graph (simulate_turn !initial_graph);
    print_graph (simulate_turn (simulate_turn !initial_graph))
  end

