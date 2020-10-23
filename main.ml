open Format

let prompt () =
  printf "%!";
  let _ = read_line () in
  ()

let find_note note = List.find (fun n -> n.Note.name = note) Note.scale

let find_interval interval =
  List.filter (fun i -> i.Interval.name = interval) Interval.intervals

let find_interval_by_nb nbn nbt =
  List.find
    (fun i -> i.Interval.nb_notes = nbn && i.nb_tones = nbt)
    Interval.intervals

let find_interval_by_name iname nbt =
  List.find
    (fun i -> i.Interval.name = iname && i.nb_tones = nbt)
    Interval.intervals

let find_opposite_interval interval =
  find_interval_by_nb (9 - interval.Interval.nb_notes) (6. -. interval.nb_tones)

let rec skip item lst =
  match lst with
  | hd :: tl -> if hd = item then lst else skip item tl
  | [] -> []

let rec find_nth_note n t lst =
  match lst with
  | hd :: tl ->
      if n = 1 then (hd, t) else find_nth_note (n - 1) (t +. hd.Note.tone_up) tl
  | [] -> find_nth_note n t Note.scale

let rec count_diff n2 cn ct lst =
  match lst with
  | hd :: tl ->
      if hd = n2 then (cn, ct)
      else count_diff n2 (cn + 1) (ct +. hd.Note.tone_up) tl
  | [] -> count_diff n2 cn ct Note.scale

let find_up_note origin interval_name =
  let origin_n = find_note origin in
  let or_scale = skip origin_n Note.scale in
  let looked_intervals = find_interval interval_name in
  let looked_nb_notes = (List.hd looked_intervals).Interval.nb_notes in
  let dest, nb_tones = find_nth_note looked_nb_notes 0.0 or_scale in
  let interval = find_interval_by_name interval_name nb_tones in
  (dest, interval)

let find_down_note origin interval_name =
  let _, interval2 = find_up_note origin interval_name in
  let interval_reverse = find_opposite_interval interval2 in
  let dest, inter_rev = find_up_note origin interval_reverse.Interval.name in
  (dest, find_interval_by_name interval_name (6. -. inter_rev.nb_tones))

let find_intervals n1 n2 =
  let n1 = find_note n1 in
  let n2 = find_note n2 in
  let nb_notes, nb_tones = count_diff n2 1 0. (skip n1 Note.scale) in
  let i1 = find_interval_by_nb nb_notes nb_tones in
  (i1, find_opposite_interval i1)

let rec go_round o c i acc func =
  if c = o then List.rev acc
  else
    let dest, inter = func c i in
    go_round o dest.Note.name i ((c, dest, inter) :: acc) func

let go_round_up origin interval =
  let c, i = find_up_note origin interval in
  (go_round origin c.Note.name interval [ (origin, c, i) ] find_up_note, true)

let go_round_down origin interval =
  let c, i = find_down_note origin interval in
  (go_round origin c.Note.name interval [ (origin, c, i) ] find_down_note, false)

let random_note () =
  let n = List.nth Note.scale (Random.int (List.length Note.scale)) in
  n.Note.name

let random_interval () =
  let i = List.nth Interval.intervals (Random.int (List.length Interval.intervals)) in
  i.Interval.name

let main lang =
  Random.self_init ();
  let transl = match lang with `En -> Transl.en | `Fr -> Transl.fr in
  let n1 = random_note () in
  let n2 = random_note () in
  let i = random_interval () in
  printf "Solfege\n";
  printf "==========";
  prompt ();
  printf "%a" transl#q_find_up_note (n1, i);
  prompt ();
  let dest, interval = find_up_note n1 i in
  printf "%a" transl#a_find_up_note (n1, dest, interval);
  printf "==========";
  prompt ();
  printf "%a" transl#q_find_down_note (n1, i);
  prompt ();
  let dest, interval = find_down_note n1 i in
  printf "%a" transl#a_find_down_note (n1, dest, interval);
  printf "==========";
  prompt ();
  printf "%a" transl#q_find_intervals (n1, n2);
  prompt ();
  let intervals = find_intervals n1 n2 in
  printf "%a" transl#a_find_intervals (n1, n2, intervals);
  printf "==========";
  prompt ();
  printf "%a" transl#q_go_round (n1, i, true);
  prompt ();
  let lst, _ = go_round_up n1 i in
  transl#a_go_round (lst, true);
  printf "==========";
  prompt ();
  printf "%a" transl#q_go_round (n1, i, false);
  prompt ();
  let lst, _ = go_round_down n1 i in
  transl#a_go_round (lst, false);
  printf "==========\n"

open Cmdliner

let lang = Arg.enum [ ("en", `En); ("fr", `Fr) ]

let cmd =
  let a_lang =
    let doc = "Language. Either English ('en') or French ('fr')." in
    Arg.(value & opt lang `En & info [ "lang" ] ~doc)
  in
  let doc = "Learn your intervals." in
  Term.(pure main $ a_lang, info "Solfege" ~doc)

let () = Term.exit @@ Term.eval cmd
