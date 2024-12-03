(* Leer los reportes desde el archivo *)
let read_reports filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic |> String.trim in
      let levels = List.map int_of_string (String.split_on_char ' ' line) in
      loop (levels :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []

(* Verificar si un reporte es seguro *)
let is_safe levels =
  let rec check adj_diff increasing decreasing = function
    | x1 :: (x2 :: _ as rest) ->
        let diff = x2 - x1 in
        if abs diff < 1 || abs diff > 3 then false
        else if diff > 0 then check adj_diff true decreasing rest
        else if diff < 0 then check adj_diff increasing true rest
        else false
    | _ -> increasing <> decreasing (* Debe ser creciente o decreciente, no ambos *)
  in
  match levels with
  | [] | [_] -> true (* Un solo nivel o vacío siempre es seguro *)
  | _ -> check true false false levels

(* Verificar si un reporte es seguro después de eliminar un nivel *)
let is_safe_with_dampener levels =
  if is_safe levels then true
  else
    List.exists (fun idx ->
      let filtered = List.filteri (fun i _ -> i <> idx) levels in
      is_safe filtered
    ) (List.init (List.length levels) Fun.id)

(* Contar reportes seguros *)
let count_safe_reports reports =
  List.fold_left (fun acc report ->
    if is_safe_with_dampener report then acc + 1 else acc
  ) 0 reports

(* Main *)
let () =
  let filename = "input_reportes.txt" in
  let reports = read_reports filename in
  let safe_count = count_safe_reports reports in
  Printf.printf "Número de reportes seguros: %d\n" safe_count
