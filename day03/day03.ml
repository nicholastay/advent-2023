open Base

type part = {
  num: int;
  r: int;
  c: int;
}

type symbol = { 
  symbol: char;
  r: int;
  c: int;
}

let parse_schematic input =
  let rec handle_row r (parts,symbols,current,c) x =
    match x with
    | [] -> (parts,symbols,0,0)
    | '.' :: t -> handle_row r (parts,symbols,0,c+1) t
    | h :: t1 :: t when Char.is_digit h && Char.is_digit t1 ->
      handle_row r (parts,symbols,current*10+(Char.get_digit_exn h),c+1) (t1 :: t)
    | h :: t when Char.is_digit h ->
      let p = {num = current*10+(Char.get_digit_exn h); c = c; r = r} in
      handle_row r (p :: parts,symbols,0,c+1) t
    | h :: t ->
      let s = {symbol = h; c = c; r = r} in
      handle_row r (parts,s :: symbols,0,c+1) t
  in
  let (parts,symbols,_,_) =
    String.split_lines input
    |> List.map ~f:String.to_list
    |> List.foldi ~init:([],[],0,0) ~f:handle_row
  in
  (* Stdlib.print_endline "parts";
  parts |> List.iter ~f:(fun x -> Printf.sprintf "%d %d %d" x.num x.r x.c |> Stdlib.print_endline);
  Stdlib.print_endline "symbols";
  symbols |> List.iter ~f:(fun x -> Printf.sprintf "%c %d %d" x.symbol x.r x.c |> Stdlib.print_endline); *)
  (parts,symbols)

let num_digits (i: int) : int = (i |> Float.of_int |> Float.log10 |> Int.of_float) + 1

let is_adjacent (p: part) (s: symbol) : bool =
  let minc = p.c - (num_digits p.num) in
  s.r >= p.r-1 
  && s.r <= p.r+1
  && s.c >= minc
  && s.c <= p.c+1

let find_non_adjacent_parts (parts,symbols) =
  let has_adjacent (p: part) = List.exists ~f:(is_adjacent p) symbols in
  List.filter ~f:has_adjacent parts

let find_gear_ratios (parts,symbols) =
  let adjacent_parts (s: symbol) = List.filter ~f:(Fn.flip is_adjacent s) parts in
  symbols
    |> List.filter ~f:(fun s -> Char.equal s.symbol '*')
    |> List.map ~f:adjacent_parts
    |> List.filter ~f:(Fn.compose ((=) 2) List.length)
    |> List.map ~f:(Fn.compose List.fold ~init:1 ~f:( * ) @@ List.map ~f:(fun p -> p.num))

let () =
  let argv = Sys.get_argv () in
  let filename = argv.(1) in
  let part2mode = (Array.length argv) >= 3 && String.equal argv.(2) "2" in
  let input = In_channel.with_open_bin filename In_channel.input_all |> String.strip in
  parse_schematic input
  |> (if part2mode then find_gear_ratios else (Fn.compose List.map ~f:(fun p -> p.num)) find_non_adjacent_parts)
  |> List.fold ~init:0 ~f:(+)
  |> Stdlib.print_int |> Stdlib.print_newline
  (* |> List.iter ~f:(fun x -> Printf.sprintf "%d %d %d" x.num x.r x.c |> Stdlib.print_endline) *)