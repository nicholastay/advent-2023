open Base

type card = {
  winning: int list;
  numbers: int list;
}

let parse_numbers s =
  String.strip s
  |> String.split ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:Int.of_string

(* e.g. Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53 *)
let split_card_line s =
  String.lsplit2 s ~on:':' |> Option.value_exn |> snd |> String.lstrip
  |> String.lsplit2 ~on:'|' |> Option.value_exn

let parse_card s =
  let (w,n) = split_card_line s in
  {winning = parse_numbers w; numbers = parse_numbers n}

let count_winning card =
  List.cartesian_product card.winning card.numbers
  |> List.count ~f:(fun (x,y) -> Int.equal x y)

let part1 cards =
  cards
  |> List.map ~f:count_winning |> List.filter ~f:(Fn.non @@ Int.equal 0)
  |> List.sum (module Int) ~f:(Fn.compose (Int.pow 2) @@ Fn.flip (-) 1)

let part2 cards =
  let n = List.length cards in
  let rec aux i cs dupe count =
    match cs with
    | [] -> count
    | h :: t ->
      let wc = count_winning h in
      let d = List.hd_exn dupe in
      let dupe = List.tl_exn dupe |> List.mapi ~f:(fun j x -> if j < wc then x+d+1 else x) in
      aux (i+1) t dupe (count+1+d)
  in
  aux 0 cards (List.init ~f:(Fn.const 0) n) 0

let () =
  let argv = Sys.get_argv () in
  let filename = argv.(1) in
  let part2mode = (Array.length argv) >= 3 && String.equal argv.(2) "2" in
  let input = In_channel.with_open_bin filename In_channel.input_all |> String.strip in
  String.split_lines input
  |> List.map ~f:parse_card
  |> (if part2mode then part2 else part1)
  |> Stdlib.print_int |> Stdlib.print_newline