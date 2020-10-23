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

let find_up_note n1 _ i =
  let or_scale = skip n1 Note.scale in
  let looked_intervals = find_interval i in
  let looked_nb_notes = (List.hd looked_intervals).Interval.nb_notes in
  let dest, nb_tones = find_nth_note looked_nb_notes 0.0 or_scale in
  let interval = find_interval_by_name i nb_tones in
  [ (n1, dest, interval) ]

let find_down_note n1 _ i =
  let _, _, interval2 = List.hd (find_up_note n1 () i) in
  let interval_reverse = find_opposite_interval interval2 in
  let _, dest, inter_rev =
    List.hd (find_up_note n1 () interval_reverse.Interval.name)
  in
  [ (n1, dest, find_interval_by_name i (6. -. inter_rev.nb_tones)) ]

let find_intervals n1 n2 _ =
  let nb_notes, nb_tones = count_diff n2 1 0. (skip n1 Note.scale) in
  let i1 = find_interval_by_nb nb_notes nb_tones in
  [ (n1, n2, i1); (n1, n2, find_opposite_interval i1) ]

let rec go_round n1 current i acc func =
  if current = n1 then List.rev acc
  else
    let _, dest, interval = List.hd (func current () i) in
    go_round n1 dest i ((current, dest, interval) :: acc) func

let go_round_up n1 _ i =
  let _, current, interval = List.hd (find_up_note n1 () i) in
  go_round n1 current i [ (n1, current, interval) ] find_up_note

let go_round_down n1 _ i =
  let _, current, interval = List.hd (find_down_note n1 () i) in
  go_round n1 current i [ (n1, current, interval) ] find_down_note
