open Format
open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt

let modify_elem id content =
  let elem = Dom_html.getElementById id in
  elem##.textContent := Js.some (Js.string content)

let show_answer_btn () =
  let elem = Dom_html.getElementById "show_answer" in
  elem##.style##.display := Js.string "block"

let hide_answer_btn () =
  let elem = Dom_html.getElementById "show_answer" in
  elem##.style##.display := Js.string "none"

let clear_answer () = modify_elem "answer" ""

let setup_buttons transl =
  let nq_btn = Dom_html.getElementById "new_question" in
  nq_btn##.textContent := Js.some (Js.string (transl#new_question_btn ()));
  nq_btn##.style##.visibility := Js.string "visible";
  let sa_btn = Dom_html.getElementById "show_answer" in
  sa_btn##.textContent := Js.some (Js.string (transl#show_answer_btn ()));
  sa_btn##.style##.display := Js.string "none"

let new_question_args transl =
  let n1 = Playground.random_note () in
  let n2 = Playground.random_note () in
  let i = Playground.random_interval () in
  let q = Playground.random_question (Question.questions transl) in
  (n1, n2, i, q)

let wait_button_q () =
  let* _ = Lwt_js_events.click (Dom_html.getElementById "new_question") in
  Lwt.return ()

let wait_button_ans () =
  let* _ = Lwt_js_events.click (Dom_html.getElementById "show_answer") in
  Lwt.return ()

let rec new_question transl =
  clear_answer ();
  show_answer_btn ();
  let n1, n2, i, q = new_question_args transl in
  let qt = asprintf "%a" q.q (n1, n2, i, q.up) in
  modify_elem "question" qt;
  let t1 =
    let* () = wait_button_q () in
    Lwt.return `New_q
  in
  let t2 =
    let* () = wait_button_ans () in
    Lwt.return `Show_ans
  in
  let* r = Lwt.pick [ t1; t2 ] in
  match r with
  | `New_q -> new_question transl
  | `Show_ans -> show_answer transl n1 n2 i q

and show_answer transl n1 n2 i q =
  hide_answer_btn ();
  let at = asprintf "%a" q.a (q.func n1 n2 i, q.up) in
  modify_elem "answer" at;
  let* () = wait_button_q () in
  new_question transl

let lang = `Fr

let main () =
  Random.self_init ();
  let transl = match lang with `En -> Transl.en | `Fr -> Transl.fr in
  setup_buttons transl;
  new_question transl

let () =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        ignore (main ());
        Js._false)
