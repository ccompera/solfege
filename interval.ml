type name = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave
[@@deriving show]

type kind = Perfect | Minor | Major | Augmented | Diminished
[@@deriving show]

type t = {
	nb_notes : int;
	kind : kind;
	name : name;
	nb_tones : float;
}
[@@deriving show]

let intervals = [
	{
		nb_notes = 1;
		kind = Perfect;
		name = Unison;
		nb_tones = 0.0;
	};
	{
		nb_notes = 2;
		kind = Minor;
		name = Second;
		nb_tones = 0.5;
	};
	{
		nb_notes = 2;
		kind = Major;
		name = Second;
		nb_tones = 1.0;
	};
	{
		nb_notes = 3;
		kind =Minor;
		name = Third;
		nb_tones = 1.5;
	};
	{
		nb_notes = 3;
		kind =Major;
		name = Third;
		nb_tones = 2.0;
	};
	{
		nb_notes = 4;
		kind = Perfect;
		name = Fourth;
		nb_tones = 2.5;
	};
	{
		nb_notes = 4;
		kind = Augmented ;
		name = Fourth;
		nb_tones = 3.0;
	};
	{
		nb_notes = 5;
		kind = Diminished;
		name = Fifth;
		nb_tones = 3.0;
	};
	{
		nb_notes = 5;
		kind = Perfect;
		name = Fifth;
		nb_tones = 3.5;
	};
	{
		nb_notes = 6;
		kind = Minor;
		name = Sixth;
		nb_tones = 4.0;
	};
	{
		nb_notes = 6;
		kind = Major;
		name = Sixth;
		nb_tones = 4.5;
	};
	{
		nb_notes = 7;
		kind = Minor;
		name = Seventh;
		nb_tones = 5.0;
	};
	{
		nb_notes = 7;
		kind = Major;
		name = Seventh;
		nb_tones = 5.5;
	};
	{
		nb_notes = 8;
		kind = Perfect;
		name = Octave;
		nb_tones = 6.0;
	}
]
