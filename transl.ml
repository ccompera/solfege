let fpf = Format.fprintf

type t =
  < start : Format.formatter -> unit -> unit
  ; leave : Format.formatter -> unit -> unit
  ; note_name : Note.t -> string
  ; interval_name : Interval.name -> string
  ; interval_kind : Interval.kind -> string
  ; print_interval :
      Format.formatter -> Note.t * Note.t * Interval.t * bool -> unit
  ; q_find_note :
      Format.formatter -> Note.t * Note.t * Interval.name * bool -> unit
  ; q_find_intervals :
      Format.formatter -> Note.t * Note.t * Interval.name * bool -> unit
  ; q_go_round :
      Format.formatter -> Note.t * Note.t * Interval.name * bool -> unit
  ; a_find_note :
      Format.formatter -> (Note.t * Note.t * Interval.t) list * bool -> unit
  ; a_find_intervals :
      Format.formatter -> (Note.t * Note.t * Interval.t) list * bool -> unit
  ; a_go_round :
      Format.formatter -> (Note.t * Note.t * Interval.t) list * bool -> unit
  ; pp_note : Format.formatter -> Note.t -> unit >

let header =
  "███████╗ ██████╗ ██╗     \
   ███████╗███████╗ ██████╗ \
   ███████╗\n\
   ██╔════╝██╔═══██╗██║     \
   ██╔════╝██╔════╝██╔════╝ \
   ██╔════╝\n\
   ███████╗██║   ██║██║     \
   █████╗  █████╗  ██║  \
   ███╗█████╗  \n\
   ╚════██║██║   ██║██║     \
   ██╔══╝  ██╔══╝  ██║   \
   ██║██╔══╝  \n\
   ███████║╚██████╔╝███████╗██║     \
   ███████╗╚██████╔╝███████╗\n\
   ╚══════╝ ╚═════╝ \
   ╚══════╝╚═╝     ╚══════╝ \
   ╚═════╝ ╚══════╝\n\
  \                                                          \n"

let en : t =
  object (self)
    method start fmt () = fpf fmt "Ready?"

    method leave fmt () = fpf fmt "See you soon!"

    method note_name n =
      match n.Note.name with
      | Note.A -> "A"
      | B -> "B"
      | C -> "C"
      | D -> "D"
      | E -> "E"
      | F -> "F"
      | G -> "G"

    method interval_name i =
      match i with
      | Interval.Unison -> "unison"
      | Second -> "second"
      | Third -> "third"
      | Fourth -> "fourth"
      | Fifth -> "fifth"
      | Sixth -> "sixth"
      | Seventh -> "seventh"
      | Octave -> "octave"

    method interval_kind i =
      match i with
      | Interval.Perfect -> "perfect"
      | Minor -> "minor"
      | Major -> "major"
      | Augmented -> "augmented"
      | Diminished -> "diminished"

    method print_interval fmt (n1, n2, i, up) =
      if up = true then
        fpf fmt "%s.•°%s is an ascending %s %s (%1.1f tone%s)"
          (self#note_name n1) (self#note_name n2)
          (self#interval_kind i.Interval.kind)
          (self#interval_name i.name)
          i.nb_tones
          (if i.nb_tones > 1. then "s" else "")
      else
        fpf fmt "%s°•.%s is a descending %s %s (%1.1f tone%s)"
          (self#note_name n1) (self#note_name n2)
          (self#interval_kind i.Interval.kind)
          (self#interval_name i.name)
          i.nb_tones
          (if i.nb_tones > 1. then "s" else "")

    method q_find_note fmt (n1, _, i, up) =
      fpf fmt "What is the %s %s of %s?"
        (if up then "ascending" else "descending")
        (self#interval_name i) (self#note_name n1)

    method q_find_intervals fmt (n1, n2, _, _) =
      fpf fmt "What are the possible intervals between %s and %s?"
        (self#note_name n1) (self#note_name n2)

    method q_go_round fmt (n1, _, i, up) =
      fpf fmt "Go %s from %s to %s starting from %s:"
        (if up then "upward" else "downward")
        (self#interval_name i) (self#interval_name i) (self#note_name n1)

    method a_find_note fmt (lst, up) =
      let n1, n2, i = List.hd lst in
      fpf fmt " %s\n%a\n" (self#note_name n2) self#print_interval (n1, n2, i, up)

    method a_find_intervals fmt (lst, _) =
      let n1, n2, i1 = List.hd lst in
      let _, _, i2 = List.nth lst 1 in
      fpf fmt "%a\n%a\n<>\n%a\n%a\n" self#print_interval (n1, n2, i1, true)
        self#print_interval (n2, n1, i2, true) self#print_interval
        (n1, n2, i2, false) self#print_interval (n2, n1, i1, false)

    method a_go_round fmt (lst, up) =
      List.iter
        (fun (n1, n2, i) -> fpf fmt "%a\n" self#print_interval (n1, n2, i, up))
        lst

    method pp_note fmt n = fpf fmt "%s" (self#note_name n)
  end

let fr : t =
  object (self)
    method start fmt () = fpf fmt "Prêt ?"

    method leave fmt () = fpf fmt "À bientôt !"

    method note_name n =
      match n.Note.name with
      | Note.A -> "la"
      | B -> "si"
      | C -> "do"
      | D -> "ré"
      | E -> "mi"
      | F -> "fa"
      | G -> "sol"

    method interval_name i =
      match i with
      | Interval.Unison -> "unison"
      | Second -> "seconde"
      | Third -> "tierce"
      | Fourth -> "quarte"
      | Fifth -> "quinte"
      | Sixth -> "sixte"
      | Seventh -> "septième"
      | Octave -> "octave"

    method interval_kind i =
      match i with
      | Interval.Perfect -> "juste"
      | Minor -> "mineure"
      | Major -> "majeure"
      | Augmented -> "augmentée"
      | Diminished -> "diminuée"

    method print_interval fmt (n1, n2, i, up) =
      if up = true then
        fpf fmt "%s.•°%s est une %s %s montante (%1.1f ton%s)"
          (self#note_name n1) (self#note_name n2)
          (self#interval_name i.Interval.name)
          (self#interval_kind i.kind)
          i.nb_tones
          (if i.nb_tones > 1. then "s" else "")
      else
        fpf fmt "%s°•.%s est une %s %s descendante (%1.1f ton%s)"
          (self#note_name n1) (self#note_name n2)
          (self#interval_name i.Interval.name)
          (self#interval_kind i.kind)
          i.nb_tones
          (if i.nb_tones > 1. then "s" else "")

    method q_find_note fmt (n1, _, i, up) =
      fpf fmt "Quelle est la %s %s de %s ?" (self#interval_name i)
        (if up then "montante" else "descendante")
        (self#note_name n1)

    method q_find_intervals fmt (n1, n2, _, _) =
      fpf fmt "Quels sont les intervalles possibles entre %s et %s ?"
        (self#note_name n1) (self#note_name n2)

    method q_go_round fmt (n1, _, i, up) =
      fpf fmt "Aller de %s en %s %s à partir de %s :" (self#interval_name i)
        (self#interval_name i)
        (if up then "montante" else "descendante")
        (self#note_name n1)

    method a_find_note fmt (lst, up) =
      let n1, n2, i = List.hd lst in
      fpf fmt " %s\n%a\n" (self#note_name n2) self#print_interval (n1, n2, i, up)

    method a_find_intervals fmt (lst, _) =
      let n1, n2, i1 = List.hd lst in
      let _, _, i2 = List.nth lst 1 in
      fpf fmt "%a\n%a\n<>\n%a\n%a\n" self#print_interval (n1, n2, i1, true)
        self#print_interval (n2, n1, i2, true) self#print_interval
        (n1, n2, i2, false) self#print_interval (n2, n1, i1, false)

    method a_go_round fmt (lst, up) =
      List.iter
        (fun (n1, n2, i) ->
          fpf fmt "%s -> %a\n" (self#note_name n2) self#print_interval
            (n1, n2, i, up))
        lst

    method pp_note fmt n = fpf fmt "%s" (self#note_name n)
  end
