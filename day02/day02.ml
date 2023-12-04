open Base

(* e.g. 3 green, 4 blue, 1 red --> Returns r,g,b *)
let parse_shown s =
  let parse_count_colour = fun (r,g,b) x ->
    let (n, label) = String.lsplit2 ~on:' ' x |> Option.value_exn in
    let n = Int.of_string n in
    match label with
    | "red" -> (r+n,g,b)
    | "green" -> (r,g+n,b)
    | "blue" -> (r,g,b+n)
    | _ -> assert false
  in
  String.split ~on:',' s
  |> List.map ~f:String.lstrip
  |> List.fold ~init:(0,0,0) ~f:parse_count_colour

(* e.g. Game 1: 3 green, 4 blue, 1 red; 3 green, 4 blue, 1 red *)
let parse_line s =
  let shown_only = String.lsplit2 s ~on:':' |> Option.value_exn |> snd |> String.lstrip in
  String.split ~on:';' shown_only
  |> List.map ~f:parse_shown

(* Returns game number if possible, else 0 *)
let game_line_part1 i s : int =
  if
    parse_line s
    |> List.exists ~f:(fun (r,g,b) -> r > 12 || g > 13 || b > 14)
  then 0 else i+1

let game_line_part2 s : int =
  let (r,g,b) =
    parse_line s
    |> List.fold ~init:(0,0,0) ~f:(fun (ar,ag,ab) (r,g,b) -> (max ar r, max ag g, max ab b))
  in
  r * g * b

let () =
  let argv = Sys.get_argv () in
  let filename = argv.(1) in
  let part2mode = (Array.length argv) >= 3 && String.equal argv.(2) "2" in
  let input = In_channel.with_open_bin filename In_channel.input_all |> String.strip in
  String.split_lines input
  |> (if part2mode then List.map ~f:game_line_part2 else List.mapi ~f:game_line_part1)
  |> List.fold ~init:0 ~f:(+)
  |> Stdlib.print_int |> Stdlib.print_newline