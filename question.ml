type kind =
  | Find_note
  | Find_intervals
  | Go_round

type t = {
  kind : kind;
  up : bool;
  func :
    Note.t -> Note.t -> Interval.name -> (Note.t * Note.t * Interval.t) list;
  q : Format.formatter -> Note.t * Note.t * Interval.name * bool -> unit;
  a : Format.formatter -> (Note.t * Note.t * Interval.t) list * bool -> unit;
}

let questions transl =
  [
    {
      kind = Find_note;
      up = true;
      func = Playground.find_up_note;
      q = transl#q_find_note;
      a = transl#a_find_note;
    };
    {
      kind = Find_note;
      up = false;
      func = Playground.find_down_note;
      q = transl#q_find_note;
      a = transl#a_find_note;
    };
    {
      kind = Find_intervals;
      up = true;
      func = Playground.find_intervals;
      q = transl#q_find_intervals;
      a = transl#a_find_intervals;
    };
    {
      kind = Go_round;
      up = true;
      func = Playground.go_round_up;
      q = transl#q_go_round;
      a = transl#a_go_round;
    };
    {
      kind = Go_round;
      up = false;
      func = Playground.go_round_down;
      q = transl#q_go_round;
      a = transl#a_go_round;
    };
  ]
