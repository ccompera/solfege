let fpf = Format.fprintf

type t =
  < note_name : Note.name -> string
  ; interval_name : Interval.name -> string
  ; interval_kind : Interval.kind -> string
  ; q_find_up_note : Format.formatter -> (Note.name * Interval.name) -> unit
  ; a_find_up_note :
      Format.formatter -> (Note.name * Note.name * Interval.t) -> unit
  ; q_find_down_note : Format.formatter -> (Note.name * Interval.name) -> unit
  ; a_find_down_note :
      Format.formatter -> (Note.name * Note.name * Interval.t) -> unit
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

    method q_find_up_note fmt (n, i) =
      fpf fmt "What is the ascending %s of %s?\n" (self#interval_name i)
        (self#note_name n)

    method a_find_up_note fmt (n1, n2, i) =
      fpf fmt "%s.•°%s is a ascending %s %s (%1.1f tones).\n"
        (self#note_name n1) (self#note_name n2)
        (self#interval_kind i.Interval.kind)
        (self#interval_name i.name)
        i.nb_tones

    method q_find_down_note fmt (n, i) =
      fpf fmt "What is the descending %s of %s?\n" (self#interval_name i)
        (self#note_name n)

    method a_find_down_note fmt (n1, n2, i) =
      fpf fmt "%s°•.%s is a descending %s %s (%1.1f tones).\n"
        (self#note_name n1) (self#note_name n2)
        (self#interval_kind i.Interval.kind)
        (self#interval_name i.name)
        i.nb_tones

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

    method q_find_up_note fmt (n, i) =
      fpf fmt "Quelle est la %s montante de %s ?\n" (self#interval_name i)
        (self#note_name n)

    method a_find_up_note fmt (n1, n2, i) =
      fpf fmt "%s.•°%s est une %s %s montante (%1.1f tons).\n"
        (self#note_name n1) (self#note_name n2)
        (self#interval_name i.Interval.name)
        (self#interval_kind i.kind)
        i.nb_tones

    method q_find_down_note fmt (n, i) =
      fpf fmt "Quelle est la %s descendante de %s ?\n" (self#interval_name i)
        (self#note_name n)

    method a_find_down_note fmt (n1, n2, i) =
      fpf fmt "%s°•.%s est une %s %s descendante (%1.1f tons).\n"
        (self#note_name n1) (self#note_name n2)
        (self#interval_name i.Interval.name)
        (self#interval_kind i.kind)
        i.nb_tones

    method pp_note fmt n = fpf fmt "%s" (self#note_name n.Note.name)
  end
