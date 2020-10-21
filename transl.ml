let fpf = Format.fprintf

type t =
  < note_name : Note.name -> string
  ; interval_name : Interval.name -> string
  ; interval_kind : Interval.kind -> string
  ; print_interval :
      Format.formatter -> Note.name * Note.name * Interval.t * bool -> unit
  ; q_find_up_note : Format.formatter -> Note.name * Interval.name -> unit
  ; a_find_up_note :
      Format.formatter -> Note.name * Note.name * Interval.t -> unit
  ; q_find_down_note : Format.formatter -> Note.name * Interval.name -> unit
  ; a_find_down_note :
      Format.formatter -> Note.name * Note.name * Interval.t -> unit
  ; q_find_intervals : Format.formatter -> Note.name * Note.name -> unit
  ; a_find_intervals :
      Format.formatter ->
      Note.name * Note.name * (Interval.t * Interval.t) ->
      unit
  ; pp_note : Format.formatter -> Note.t -> unit >

let en : t =
  object (self)
    method note_name n =
      match n with
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

    method q_find_up_note fmt (n, i) =
      fpf fmt "What is the ascending %s of %s?" (self#interval_name i)
        (self#note_name n)

    method a_find_up_note fmt (n1, n2, i) =
      fpf fmt " %s\n%a.\n" (self#note_name n2) self#print_interval
        (n1, n2, i, true)

    method q_find_down_note fmt (n, i) =
      fpf fmt "What is the descending %s of %s?" (self#interval_name i)
        (self#note_name n)

    method a_find_down_note fmt (n1, n2, i) =
      fpf fmt " %s\n%a.\n"
        (self#note_name n2) self#print_interval (n1, n2, i, false)

    method q_find_intervals fmt (n1, n2) =
      fpf fmt "What are the possible intervals between %s and %s?"
        (self#note_name n1) (self#note_name n2)

    method a_find_intervals fmt (n1, n2, (i1, i2)) =
      fpf fmt "%a.\n%a.\n<>\n%a\n%a\n" self#print_interval (n1, n2, i1, true)
        self#print_interval (n2, n1, i2, true) self#print_interval
        (n1, n2, i2, false) self#print_interval (n2, n1, i1, false)

    method pp_note fmt n = fpf fmt "%s" (self#note_name n.Note.name)
  end

let fr : t =
  object (self)
    method note_name n =
      match n with
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

    method q_find_up_note fmt (n, i) =
      fpf fmt "Quelle est la %s montante de %s ?" (self#interval_name i)
        (self#note_name n)

    method a_find_up_note fmt (n1, n2, i) =
      fpf fmt " %s\n%a.\n" (self#note_name n2) self#print_interval
        (n1, n2, i, true)

    method q_find_down_note fmt (n, i) =
      fpf fmt "Quelle est la %s descendante de %s ?" (self#interval_name i)
        (self#note_name n)

    method a_find_down_note fmt (n1, n2, i) =
      fpf fmt " %s\n%a.\n" (self#note_name n2) self#print_interval
        (n1, n2, i, false)

    method q_find_intervals fmt (n1, n2) =
      fpf fmt "Quels sont les intervalles possibles entre %s et %s ?"
        (self#note_name n1) (self#note_name n2)

    method a_find_intervals fmt (n1, n2, (i1, i2)) =
      fpf fmt "%a.\n%a.\n<>\n%a\n%a\n" self#print_interval (n1, n2, i1, true)
        self#print_interval (n2, n1, i2, true) self#print_interval
        (n1, n2, i2, false) self#print_interval (n2, n1, i1, false)

    method pp_note fmt n = fpf fmt "%s" (self#note_name n.Note.name)
  end
