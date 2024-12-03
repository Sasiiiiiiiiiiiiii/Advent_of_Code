(* Expresi贸n regular para instrucciones v谩lidas de la forma mul(X,Y) *)
let valid_mul_regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"

(* Funci贸n para extraer todas las coincidencias v谩lidas y calcular sus productos *)
let process_memory memory =
  let rec extract_and_sum acc pos =
    try
      ignore (Str.search_forward valid_mul_regex memory pos);
      let x = int_of_string (Str.matched_group 1 memory) in
      let y = int_of_string (Str.matched_group 2 memory) in
      extract_and_sum (acc + (x * y)) (Str.match_end ())
    with Not_found -> acc
  in
  extract_and_sum 0 0

(* Leer el archivo de entrada y procesar la memoria *)
let () =
  let filename = "input_memory.txt" in
  let ic = open_in filename in
  let memory = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let result = process_memory memory in
  Printf.printf "El resultado total de las multiplicaciones es: %d\n" result
