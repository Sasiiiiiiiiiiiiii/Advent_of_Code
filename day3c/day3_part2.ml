(* Expresiones regulares para las instrucciones relevantes *)
let valid_mul_regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"
let do_regex = Str.regexp "do()"
let dont_regex = Str.regexp "don't()"

(* Procesar la memoria *)
let process_memory memory =
  let rec extract_and_sum acc pos enabled =
    try
      (* Buscar las instrucciones relevantes *)
      let next_mul = try Some (Str.search_forward valid_mul_regex memory pos) with Not_found -> None in
      let next_do = try Some (Str.search_forward do_regex memory pos) with Not_found -> None in
      let next_dont = try Some (Str.search_forward dont_regex memory pos) with Not_found -> None in

      (* Determinar cuál instrucción está más cerca *)
      let next_instr, next_pos =
        match next_mul, next_do, next_dont with
        | Some mp, Some dp, Some dt ->
            if mp < dp && mp < dt then (`Mul, mp)
            else if dp < dt then (`Do, dp)
            else (`Dont, dt)
        | Some mp, Some dp, None -> if mp < dp then (`Mul, mp) else (`Do, dp)
        | Some mp, None, Some dt -> if mp < dt then (`Mul, mp) else (`Dont, dt)
        | None, Some dp, Some dt -> if dp < dt then (`Do, dp) else (`Dont, dt)
        | Some mp, None, None -> (`Mul, mp)
        | None, Some dp, None -> (`Do, dp)
        | None, None, Some dt -> (`Dont, dt)
        | None, None, None -> raise Not_found
      in

      (* Procesar la instrucción *)
      match next_instr with
      | `Mul ->
          (* Validar coincidencia antes de acceder a los grupos *)
          if Str.string_match valid_mul_regex memory next_pos then
            let x = int_of_string (Str.matched_group 1 memory) in
            let y = int_of_string (Str.matched_group 2 memory) in
            let new_acc = if enabled then acc + (x * y) else acc in
            extract_and_sum new_acc (Str.match_end ()) enabled
          else
            extract_and_sum acc (next_pos + 1) enabled
      | `Do -> extract_and_sum acc (next_pos + 4) true
      | `Dont -> extract_and_sum acc (next_pos + 6) false
    with Not_found -> acc
  in
  extract_and_sum 0 0 true

(* Leer el archivo de entrada y procesar la memoria *)
let () =
  let filename = "input_memory.txt" in
  let ic = open_in filename in
  let memory = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let result = process_memory memory in
  Printf.printf "El resultado total de las multiplicaciones es: %d\n" result
