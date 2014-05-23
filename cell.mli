type coord =
  | North
  | East
  | South
  | West

type cell
exception No_closed_doors

val nb_doors		: int
val create		: int -> cell
val open_door		: cell -> coord -> unit
val is_door_open	: cell -> coord -> bool
val rand_closed_door	: cell -> coord
val iter		: (coord -> unit) -> cell -> unit
val opposite_door	: coord -> coord
val get_color		: cell -> int
val set_color		: cell -> int -> unit
