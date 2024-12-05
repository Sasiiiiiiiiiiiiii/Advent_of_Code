(* Verificar si hay un "X-MAS" en una posición específica *)
let is_xmas matrix rows cols r c =
  let in_bounds r c = r >= 0 && r < rows && c >= 0 && c < cols in
  let check_diagonal1 =
    in_bounds (r - 1) (c - 1) && in_bounds (r) (c) && in_bounds (r + 1) (c + 1) &&
    ((matrix.(r - 1).(c - 1) = 'M' && matrix.(r).(c) = 'A' && matrix.(r + 1).(c + 1) = 'S') ||
     (matrix.(r - 1).(c - 1) = 'S' && matrix.(r).(c) = 'A' && matrix.(r + 1).(c + 1) = 'M'))
  in
  let check_diagonal2 =
    in_bounds (r - 1) (c + 1) && in_bounds (r) (c) && in_bounds (r + 1) (c - 1) &&
    ((matrix.(r - 1).(c + 1) = 'M' && matrix.(r).(c) = 'A' && matrix.(r + 1).(c - 1) = 'S') ||
     (matrix.(r - 1).(c + 1) = 'S' && matrix.(r).(c) = 'A' && matrix.(r + 1).(c - 1) = 'M'))
  in
  check_diagonal1 && check_diagonal2

(* Contar todas las apariciones del patrón "X-MAS" en la matriz *)
let count_xmas matrix rows cols =
  let count = ref 0 in
  for r = 1 to rows - 2 do
    for c = 1 to cols - 2 do
      if is_xmas matrix rows cols r c then incr count
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
  let result = count_xmas matrix rows cols in
  Printf.printf "El patrón 'X-MAS' aparece %d veces en la matriz.\n" result
