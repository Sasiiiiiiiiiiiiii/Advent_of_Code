(* Encuentra el índice de un elemento en una lista *)
let index_of elem lst =
  let rec aux idx = function
    | [] -> raise Not_found
    | x :: xs -> if x = elem then idx else aux (idx + 1) xs
  in
  aux 0 lst

(* Toma los primeros n elementos de una lista *)
let rec take n lst =
  match lst, n with
  | [], _ -> []
  | _, 0 -> []
  | x :: xs, _ -> x :: take (n - 1) xs

(* Elimina los primeros n elementos de una lista *)
let rec drop n lst =
  match lst, n with
  | [], _ -> []
  | _, 0 -> lst
  | _ :: xs, _ -> drop (n - 1) xs

(* Parsear las reglas *)
let parse_rules lines =
  List.fold_left (fun acc line ->
    match String.split_on_char '|' line with
    | [a; b] -> (int_of_string a, int_of_string b) :: acc
    | _ -> failwith "Formato de regla inválido"
  ) [] lines

(* Parsear las actualizaciones *)
let parse_updates lines =
  List.map (fun line ->
    List.map int_of_string (String.split_on_char ',' line)
  ) lines

(* Construir el grafo a partir de las reglas *)
let build_graph rules =
  let graph = Hashtbl.create (List.length rules) in
  List.iter (fun (a, b) ->
    if Hashtbl.mem graph a then
      Hashtbl.replace graph a (b :: Hashtbl.find graph a)
    else
      Hashtbl.add graph a [b]
  ) rules;
  graph

(* Verificar si una actualización es válida según el grafo *)
let is_valid_update update graph =
  let indices = Hashtbl.create (List.length update) in
  List.iteri (fun i page -> Hashtbl.add indices page i) update;
  try
    Hashtbl.iter (fun a bs ->
      if List.mem a update then
        List.iter (fun b ->
          if List.mem b update && Hashtbl.find indices a > Hashtbl.find indices b then
            raise Exit
        ) bs
    ) graph;
    true
  with Exit -> false

(* Calcular la página central de una lista *)
let middle_page update =
  List.nth update (List.length update / 2)

(* Main *)
let () =
  let input = "input.txt" in
  let ic = open_in input in
  let rec read_lines acc =
    try read_lines (input_line ic :: acc) with End_of_file -> close_in ic; List.rev acc
  in
  let lines = read_lines [] in
  let split_idx = index_of "" lines in
  let rules, updates =
    (take split_idx lines, drop (split_idx + 1) lines)
  in
  let rules = parse_rules rules in
  let updates = parse_updates updates in
  let graph = build_graph rules in
  let valid_updates = List.filter (fun update -> is_valid_update update graph) updates in
  let middle_sum = List.fold_left (fun acc update -> acc + middle_page update) 0 valid_updates in
  Printf.printf "La suma de las páginas centrales es: %d\n" middle_sum
