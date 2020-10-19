let en =
	object
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

	end

let fr =
	object
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

	end
