let char_accum acc c =
  if acc = (-1) then
    match c with
    (* This solution with -48 is kinda whack, dunno if better way in stdlib? *)
    | '0' .. '9' -> (int_of_char c) - 48
    | _ -> acc
  else acc

(* Also probably a better way than folding so we don't have to iterate through entire str? *)
let first_num = String.fold_left char_accum (-1)
let last_num str = String.fold_right (Fun.flip char_accum) str (-1)

let first_last_num (str: string) : int =
  let a = first_num str in
  let b = last_num str in
  (a * 10) + b

(* Part 2: tactic to only do second letter so we don't mess up 'overlap' ones *)
let comp f g x = f (g x)
let word_to_number (str: string) : string =
  match str with
  | "one" -> "o1e"
  | "two" -> "t2o"
  | "three" -> "t3ree"
  | "four" -> "f4ur"
  | "five" -> "f5ve"
  | "six" -> "s6x"
  | "seven" -> "s7ven"
  | "eight" -> "e8ght"
  | "nine" -> "n9ne"
  | _ -> assert false
let word_number_regex = Str.regexp "one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"
let rec process_number_words (str: string) : string =
  try
    Str.search_forward word_number_regex str 0 |> ignore;
    process_number_words @@ Str.substitute_first word_number_regex (comp word_to_number Str.matched_string) str
  with Not_found -> str

let () =
  let part2mode = (Array.length Sys.argv) = 3 && Sys.argv.(2) = "2" in
  let filename = Sys.argv.(1) in
  let input = In_channel.with_open_bin filename In_channel.input_all |> String.trim in
  input
  |> String.split_on_char '\n'
  |> (if part2mode then List.map process_number_words else Fun.id)
  |> List.map first_last_num
  |> List.fold_left (+) 0
  |> Int.to_string
  |> print_endline