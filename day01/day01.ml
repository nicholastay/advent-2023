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

let () =
  let filename = Sys.argv.(1) in
  let input = In_channel.with_open_bin filename In_channel.input_all |> String.trim in
  String.split_on_char '\n' input
  |> List.map first_last_num
  |> List.fold_left (+) 0
  |> Int.to_string
  |> print_endline