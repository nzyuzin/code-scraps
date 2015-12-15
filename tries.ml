type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
  [('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
   ('t',
    Trie (None,
    [('e',
      Trie (None,
      [('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
       ('a', Trie (Some 3, []))]));
     ('o', Trie (Some 7, []))]));
   ('A', Trie (Some 15, []))])

let children_from_char m c =
  match List.find_all (fun (k, v) -> k = c) m with
  | [(k, t)] -> Some t
  | _ -> None

let update_children m c t =
  let len = List.length m in
  let rec inner iter m' success =
    if iter = len then
      if success = true then List.rev m'
      else List.rev ((c,t) :: m')
    else
      let (k, v) = List.nth m iter in 
      if k = c then
        inner (iter + 1) ((k, t) :: m') true
      else
        inner (iter + 1) ((k, v) :: m') success in
  inner 0 [] false

let rec lookup (Trie (c,t)) w =
  if w = "" then
    c
  else 
    match children_from_char t (String.get w 0) with
    | Some tr -> lookup tr (String.sub w 1 (String.length w - 1))
    | None -> None

let rec insert (Trie (c, t)) w v =
  if w = "" then
    Trie (Some v, t)
  else
    let chr = String.get w 0 in
    let substr = String.sub w 1 (String.length w - 1) in
    match children_from_char t chr with 
    | Some tr -> Trie (c, update_children t chr (insert tr substr v))
    | None -> Trie (c, t @ [(chr, insert empty substr v)])

(* example 
 *  insert
 *    (Trie (Some (-5),
 *      [('j', Trie (None, [('m', Trie (None, [('g', Trie (Some 3, []))]))]));
 *       ('a', Trie (None, [('d', Trie (Some (-5), []))]));
 *       ('m', Trie (None, [('p', Trie (Some (-4), []))]));
 *       ('g',
 *        Trie (Some (-5), [('g', Trie (None, [('d', Trie (Some (-3), []))]))]))]))
 *    "mp"
 *    (-4)
 *)
