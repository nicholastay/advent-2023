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

let find_non_adjacent_parts (parts,symbols) =
  let has_adjacent (part: part) : bool =
    let minc = part.c - (num_digits part.num) in
    let is_adjacent s : bool =
      ignore s.symbol;  (* get around unused warning, is there a better way? *)
      s.r >= part.r-1 
      && s.r <= part.r+1
      && s.c >= minc
      && s.c <= part.c+1
    in
    List.exists symbols ~f:is_adjacent
  in
  List.filter ~f:has_adjacent parts

let () =
  let argv = Sys.get_argv () in
  let filename = argv.(1) in
  let input = In_channel.with_open_bin filename In_channel.input_all |> String.strip in
  parse_schematic input
  |> find_non_adjacent_parts
  |> List.map ~f:(fun p -> p.num)
  |> List.fold ~init:0 ~f:(+)
  |> Stdlib.print_int |> Stdlib.print_newline
  (* |> List.iter ~f:(fun x -> Printf.sprintf "%d %d %d" x.num x.r x.c |> Stdlib.print_endline) *)