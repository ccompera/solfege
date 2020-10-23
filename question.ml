type t = {
  up : bool;
  func :
    Note.t -> Note.t -> Interval.name -> (Note.t * Note.t * Interval.t) list;
  q : Format.formatter -> Note.t * Note.t * Interval.name * bool -> unit;
  a : Format.formatter -> (Note.t * Note.t * Interval.t) list * bool -> unit;
}

let questions transl =
  [
    {
      up = true;
      func = Playground.find_up_note;
      q = transl#q_find_note;
      a = transl#a_find_note;
    };
    {
      up = false;
      func = Playground.find_down_note;
      q = transl#q_find_note;
      a = transl#a_find_note;
    };
    {
      up = true;
      func = Playground.find_intervals;
      q = transl#q_find_intervals;
      a = transl#a_find_intervals;
    };
    {
      up = true;
      func = Playground.go_round_up;
      q = transl#q_go_round;
      a = transl#a_go_round;
    };
    {
      up = false;
      func = Playground.go_round_down;
      q = transl#q_go_round;
      a = transl#a_go_round;
    };
  ]
