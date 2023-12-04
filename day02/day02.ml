open Base

(* e.g. 3 green, 4 blue, 1 red --> Returns r,g,b *)
let parse_shown s =
  let parse_count_colour = fun (r,g,b) x ->
    let sx = String.lsplit2 ~on:' ' x |> Option.value_exn in
    let n = Int.of_string @@ fst sx in
    match snd sx with
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
let game_line r g b i s : int =
  if
    parse_line s
    |> List.exists ~f:(fun (sr,sg,sb) -> sr > r || sg > g || sb > b)
  then 0 else i+1

let () =
  let argv = Sys.get_argv () in
  let filename = argv.(1) in
  let input = In_channel.with_open_bin filename In_channel.input_all |> String.strip in
  String.split_lines input
  |> List.mapi ~f:(game_line 12 13 14)
  |> List.fold ~init:0 ~f:(+)
  |> Stdlib.print_int |> Stdlib.print_newline