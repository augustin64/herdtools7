(** diymicrofuzz
Generates n_tests random tests to output_dir with the edges given as input.
You may then consider running them with herd7 to find any invalid test
$ for file in generated/*.litmus; do output=$(herd7 $file); echo output | grep error && break; done
*)

let generated_count = ref 0
let output_dir = ref "."
let n_tests = ref 10000
let max_edge_count = 7

let next_filename () =
  incr generated_count;
  Filename.concat !output_dir
    ("fuzz" ^ string_of_int !generated_count ^ ".litmus")

(** Pick a random item from a list and put it as the head *)
let shuffle_first l =
  let random_index = Random.int (List.length l) in
  let rec get_item l n =
    match l, n with
    | e :: q, 0 -> e, q
    | e :: q, _ ->
        let r, q = get_item q (n - 1) in
        r, e :: q
    | [], _ -> assert false
  in
  let e, q = get_item l random_index in
  e :: q

(** Are those directions compatible ? *)
let ( @= ) dir1 dir2 =
  match dir1, dir2 with
  | Edge.Rm false, Edge.Rm false -> true
  | Edge.Wm false, Edge.Wm false -> true
  | Edge.RegEvent, Edge.RegEvent -> true
  | Edge.Rm b1, Edge.Rm b2 -> b1 <> b2 (* xor *)
  | Edge.Wm b1, Edge.Wm b2 -> b1 <> b2
  | _, _ -> false

(** Generate n_tests "random" cycles *)
let rec backtrack edges complete_edges_bank =
  if !generated_count <= !n_tests then
    if
      List.length edges > 1
      && Random.int 2000 = 0
         (* if we were to pick all tests, we may have approximately the same cycle,
            apart from the last edge, we thus ignore a large part of these *)
      && (List.hd edges |> fst |> Edge.edge_direction |> fst)
         @= (Utils.list_last edges |> fst |> Edge.edge_direction |> snd)
      (* directions compatibility check *)
    then (
      let filename = next_filename () in
      let chan = open_out filename in
      try
        try
          Compile.to_channel edges chan;
          close_out chan
        with Misc.UserError _ ->
          (* UserError means that the input caused a crash, we just ignore it *)
          decr generated_count;
          close_out chan;
          Sys.remove filename
      with Misc.Fatal _ ->
        (* Fatal means that the program reached an invalid state *)
        close_out chan;
        (List.map Edge.pp_annotated_edge edges |> String.concat " ")
        ^ " failed\n"
        |> failwith);
  if List.length edges < max_edge_count then
    let rec try_add edges edges_bank =
      match edges, edges_bank with
      | [], eb :: qb ->
          backtrack [eb] complete_edges_bank;
          try_add edges qb
      | (e, _) :: _, (eb, ab) :: qb
        when snd (Edge.edge_direction eb) @= fst (Edge.edge_direction e) ->
          backtrack ((eb, ab) :: edges) complete_edges_bank;
          try_add edges qb
      | _, _ :: qb -> try_add edges qb
      | _, _ -> ()
    in
    try_add edges complete_edges_bank

let () =
  Random.self_init ();
  let list_iico = ref false in
  let edges_ref = ref [] in
  (* load iico edges *)
  Iico.init ();
  let parse_edge s =
    try
      edges_ref :=
        (Lexing.from_string s |> Parser.main Lexer.token) :: !edges_ref
    with Parser.Error | Not_found ->
      raise (Arg.Bad (Printf.sprintf "Unknown edge '%s'" s))
  in

  let options_list =
    [
      ( "-v",
        Arg.Unit (fun () -> incr Config.verbose),
        "Increase verbosity (use multiple times)" );
      "-list-iico", Arg.Set list_iico, "list iico[] edges";
      "-output", Arg.Set_string output_dir, "output directory";
      "-n", Arg.Set_int n_tests, "number of tests";
      ( "-debug",
        Arg.Unit (fun () -> Printexc.record_backtrace true),
        "Print backtrace on crash" );
    ]
  in
  let usage = "diymicro [options] <edge 1> <edge 2> <...>" in

  (* message d'accueil, option -help *)
  Arg.parse options_list parse_edge usage;

  if !list_iico then Edge.list_iico_edges ()
  else if !edges_ref = [] then Arg.usage options_list usage
  else
    let annot_edges = List.rev !edges_ref in
    (* We need at least one External edge in a cycle, we arbitrarily start with an Rfe *)
    backtrack [Edge.Rf Edge.External, Edge.AnnotNone] annot_edges
