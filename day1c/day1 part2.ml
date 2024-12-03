(* Función para leer el archivo y parsear las listas *)
let read_lists filename =
  let ic = open_in filename in
  let rec loop acc_left acc_right =
    try
      let line = input_line ic |> String.trim in
      let parts = List.filter ((<>) "") (String.split_on_char ' ' line) in
      match parts with
      | [left; right] ->
          loop (int_of_string left :: acc_left) (int_of_string right :: acc_right)
      | _ ->
          (* Imprimir advertencia si la línea no es válida *)
          Printf.printf "Advertencia: línea con formato no válido: %s\n" line;
          loop acc_left acc_right
    with End_of_file ->
      close_in ic;
      (List.rev acc_left, List.rev acc_right)
  in
  loop [] []



(* Función para calcular la distancia total *)
let total_distance left_list right_list =
  let sorted_left = List.sort compare left_list in
  let sorted_right = List.sort compare right_list in
  List.fold_left2 (fun acc l r -> acc + abs (l - r)) 0 sorted_left sorted_right

let () =
  let filename = "input listas.txt" in
  let (left_list, right_list) = read_lists filename in
  (* Imprimir las listas leídas *)
  Printf.printf "Lista izquierda (sin ordenar): %s\n"
    (String.concat ", " (List.map string_of_int left_list));
  Printf.printf "Lista derecha (sin ordenar): %s\n"
    (String.concat ", " (List.map string_of_int right_list));

  (* Ordenar las listas *)
  let sorted_left = List.sort compare left_list in
  let sorted_right = List.sort compare right_list in
  (* Imprimir las listas ordenadas *)
  Printf.printf "Lista izquierda (ordenada): %s\n"
    (String.concat ", " (List.map string_of_int sorted_left));
  Printf.printf "Lista derecha (ordenada): %s\n"
    (String.concat ", " (List.map string_of_int sorted_right));

  (* Calcular la distancia total *)
  let result = total_distance sorted_left sorted_right in
  Printf.printf "La distancia total es: %d\n" result
(* Crear un mapa con las frecuencias de los números en la lista derecha *)
let count_frequencies lst =
  let table = Hashtbl.create (List.length lst) in
  List.iter (fun x ->
    Hashtbl.replace table x ((Hashtbl.find_opt table x |> Option.value ~default:0) + 1)
  ) lst;
  table

(* Calcular el similarity score *)
let similarity_score left_list right_list =
  let freq_table = count_frequencies right_list in
  List.fold_left (fun acc x ->
    let count = Hashtbl.find_opt freq_table x |> Option.value ~default:0 in
    acc + (x * count)
  ) 0 left_list

(* Main *)
let () =
  let filename = "input listas.txt" in
  let (left_list, right_list) = read_lists filename in
  let score = similarity_score left_list right_list in
  Printf.printf "El similarity score es: %d\n" score
