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

(* Ordenar una actualización usando un ordenamiento topológico *)
let topological_sort update graph =
  let in_degree = Hashtbl.create (List.length update) in
  let adj_list = Hashtbl.create (List.length update) in

  (* Crear subgrafo basado en las páginas presentes en la actualización *)
  List.iter (fun node ->
    Hashtbl.add in_degree node 0;
    Hashtbl.add adj_list node []
  ) update;
  Hashtbl.iter (fun node edges ->
    if List.mem node update then
      List.iter (fun edge ->
        if List.mem edge update then (
          Hashtbl.replace adj_list node (edge :: Hashtbl.find adj_list node);
          Hashtbl.replace in_degree edge (Hashtbl.find in_degree edge + 1)
        )
      ) edges
  ) graph;

  (* Ordenamiento topológico *)
  let queue = Queue.create () in
  Hashtbl.iter (fun node degree ->
    if degree = 0 then Queue.push node queue
  ) in_degree;

  let sorted = ref [] in
  while not (Queue.is_empty queue) do
    let node = Queue.pop queue in
    sorted := node :: !sorted;
    List.iter (fun neighbor ->
      Hashtbl.replace in_degree neighbor (Hashtbl.find in_degree neighbor - 1);
      if Hashtbl.find in_degree neighbor = 0 then Queue.push neighbor queue
    ) (Hashtbl.find adj_list node)
  done;
  List.rev !sorted

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

  (* Identificar las actualizaciones incorrectas y corregirlas *)
  let invalid_updates = List.filter (fun update -> not (is_valid_update update graph)) updates in
  let corrected_updates = List.map (fun update -> topological_sort update graph) invalid_updates in
  let middle_sum = List.fold_left (fun acc update -> acc + middle_page update) 0 corrected_updates in
  Printf.printf "La suma de las páginas centrales es: %d\n" middle_sum
