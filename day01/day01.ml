open Base

let first_num x = String.find_map x ~f:Char.get_digit |> Option.value_exn
let last_num = Fn.compose first_num String.rev

let first_last_num (str: string) : int =
  let a = first_num str in
  let b = last_num str in
  (a * 10) + b

let process_number_word s nw = String.substr_replace_all ~pattern:(fst nw) ~with_:(snd nw) s
let process_number_words s : string =
  let num_words = [
    "one", "o1e";
    "two", "t2o";
    "three", "t3ree";
    "four", "f4ur";
    "five", "f5ve";
    "six", "s6x";
    "seven", "s7ven";
    "eight", "e8ght";
    "nine", "n9ne"
  ] in
  List.fold num_words ~init:s ~f:process_number_word

let () =
  let argv = Sys.get_argv () in
  let filename = argv.(1) in
  let part2mode = (Array.length argv) >= 3 && String.equal argv.(2) "2" in
  let input = In_channel.with_open_bin filename In_channel.input_all |> String.strip in
  String.split_lines input
  |> (if part2mode then List.map ~f:process_number_words else Fn.id)
  |> List.map ~f:first_last_num
  |> List.fold ~init:0 ~f:(+)
  |> Stdlib.print_int |> Stdlib.print_newline