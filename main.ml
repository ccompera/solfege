open Format

let find_note note = List.find (fun n -> n.Note.name = note) Note.scale

let find_interval interval =
  List.filter (fun i -> i.Interval.name = interval) Interval.intervals

let rec skip item lst =
  match lst with
  | hd :: tl -> if hd = item then lst else skip item tl
  | [] -> []

let rec find_nth_note n t lst =
  match lst with
  | hd :: tl ->
      if n = 1 then (hd, t) else find_nth_note (n - 1) (t +. hd.Note.tone_up) tl
  | [] -> find_nth_note n t Note.scale

let find_up_note origin interval_name =
  let origin_n = find_note origin in
  let or_scale = skip origin_n Note.scale in
  let looked_intervals = find_interval interval_name in
  let looked_nb_notes = (List.hd looked_intervals).Interval.nb_notes in
  let dest, nb_tones = find_nth_note looked_nb_notes 0.0 or_scale in
  let interval =
    List.find
      (fun i -> i.Interval.name = interval_name && i.nb_tones = nb_tones)
      Interval.intervals
  in
  (dest, interval)

let () =
  printf "Solfege@\n";
  let dest, interval = find_up_note Note.B Interval.Third in
  printf "The upcoming third of B is %a. It is a %s %s.\n" Transl.en#pp_note
    dest
    (Transl.en#interval_kind interval.Interval.kind)
    (Transl.en#interval_name interval.Interval.name);
  ()
