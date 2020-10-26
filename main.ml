open Format
open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt
let fpaf = Format.asprintf
let doc = Dom_html.document

let prompt () =
  printf "%!";
  let* _ = Lwt_js_events.click doc##.body in
  Lwt.return ()

let modify_elem id content =
  let elem = Dom_html.getElementById id in
  elem##.textContent := Js.some (Js.string content)
  (* Dom.appendChild elem (doc##createTextNode (Js.string content)); *)

let printjs id content =
  modify_elem id content;
  if id = "question" then modify_elem "answer" ""

let random_note () = List.nth Note.scale (Random.int (List.length Note.scale))

let remove_first lst = match lst with _ :: tl -> tl | [] -> []

let random_interval () =
  let is = remove_first (List.rev (remove_first Interval.intervals)) in
  let i = List.nth is (Random.int (List.length is)) in
  i.Interval.name

let random_question qs = List.nth qs (Random.int (List.length qs))

let rec playrec transl =
  let n1 = random_note () in
  let n2 = random_note () in
  let i = random_interval () in
  let q = random_question (Question.questions transl) in
  let qt = fpaf "%a" q.q (n1, n2, i, q.up) in
  printjs "question" qt;
  let* _ = prompt () in
  let at = fpaf "%a" q.a (q.func n1 n2 i, q.up) in
  printjs "answer" at;
  (* printf "=========="; *)
  let* _ = prompt () in
  playrec transl

let play transl =
  printf "%s" Transl.header;
  Playground.start transl;
  let* _ = prompt () in
  playrec transl

let lang = `Fr

let main () =
  Random.self_init ();
  let transl = match lang with `En -> Transl.en | `Fr -> Transl.fr in
  play transl

let () =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        ignore (main ());
        Js._false)
