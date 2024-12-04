(* Palabra a buscar *)
let target = "XMAS"
let target_len = String.length target

(* Función para verificar si la palabra aparece desde una posición en una dirección dada *)
let check_word matrix rows cols start_row start_col delta_row delta_col =
  let rec check idx r c =
    if idx = target_len then true
    else if r < 0 || r >= rows || c < 0 || c >= cols then false
    else if matrix.(r).(c) <> target.[idx] then false
    else check (idx + 1) (r + delta_row) (c + delta_col)
  in
  check 0 start_row start_col

(* Contar todas las apariciones de la palabra en la matriz *)
let count_word matrix rows cols =
  let directions = [ (0, 1); (0, -1); (1, 0); (-1, 0); (1, 1); (1, -1); (-1, 1); (-1, -1) ] in
  let count = ref 0 in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      List.iter (fun (dr, dc) ->
        if check_word matrix rows cols r c dr dc then
          incr count
      ) directions
    done
  done;
  !count

(* Leer la matriz desde un archivo *)
let read_matrix filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    [||] (* Nunca se llega aquí *)
  with End_of_file ->
    close_in ic;
    (* Convertir las líneas en una matriz bidimensional *)
    let lines = List.rev !lines in
    let rows = List.length lines in
    let cols = String.length (List.hd lines) in
    Array.init rows (fun r -> Array.init cols (fun c -> (List.nth lines r).[c]))

(* Main *)
let () =
  let filename = "input_wordsearch.txt" in
  let matrix = read_matrix filename in
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let result = count_word matrix rows cols in
  Printf.printf "La palabra '%s' aparece %d veces en la matriz.\n" target result
