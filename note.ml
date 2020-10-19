type name = A | B | C | D | E | F | G

type tone = Tone | Semitone

type t = {
	name : name;
	tone_up : tone;
	tone_down : tone;
}

let scale = [
	{
		name = C;
		tone_up = Tone;
		tone_down = Semitone;
	};
	{
		name = D;
		tone_up = Tone;
		tone_down = Tone;
	};
	{
		name = E;
		tone_up = Semitone;
		tone_down = Tone;
	};
	{
		name = F;
		tone_up = Tone;
		tone_down = Semitone;
	};
	{
		name = G;
		tone_up = Tone;
		tone_down = Tone;
	};
	{
		name = A;
		tone_up = Tone;
		tone_down = Tone;
	};
	{
		name = B;
		tone_up = Semitone;
		tone_down = Tone;
	}
]
