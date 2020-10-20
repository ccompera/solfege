type name = A | B | C | D | E | F | G
[@@deriving show]

type t = {
	name : name;
	tone_up : float;
	tone_down : float;
}
[@@deriving show]

let scale = [
	{
		name = C;
		tone_up = 1.0;
		tone_down = 0.5;
	};
	{
		name = D;
		tone_up = 1.0;
		tone_down = 1.0;
	};
	{
		name = E;
		tone_up = 0.5;
		tone_down = 1.0;
	};
	{
		name = F;
		tone_up = 1.0;
		tone_down = 0.5;
	};
	{
		name = G;
		tone_up = 1.0;
		tone_down = 1.0;
	};
	{
		name = A;
		tone_up = 1.0;
		tone_down = 1.0;
	};
	{
		name = B;
		tone_up = 0.5;
		tone_down = 1.0;
	}
]
