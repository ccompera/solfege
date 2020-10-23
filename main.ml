open Format

let prompt () =
  printf "%!";
  let _ = read_line () in
  ()

(* let find_note_by_name note = List.find (fun n -> n.Note.name = note) Note.scale *)

let random_note () = List.nth Note.scale (Random.int (List.length Note.scale))

let remove_first lst = match lst with _ :: tl -> tl | [] -> []

let random_interval () =
  let is = remove_first (List.rev (remove_first Interval.intervals)) in
  let i = List.nth is (Random.int (List.length is)) in
  i.Interval.name

let random_question qs = List.nth qs (Random.int (List.length qs))

let play transl =
  printf "%s" Transl.header;
  transl#start ();
  prompt ();
  while true do
    let n1 = random_note () in
    let n2 = random_note () in
    let i = random_interval () in
    let q = random_question (Question.questions transl) in
    printf "%a" q.q (n1, n2, i, q.up);
    prompt ();
    printf "%a" q.a (q.func n1 n2 i, q.up);
    printf "==========";
    prompt ()
  done

let main lang =
  Random.self_init ();
  let transl = match lang with `En -> Transl.en | `Fr -> Transl.fr in
  play transl

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
