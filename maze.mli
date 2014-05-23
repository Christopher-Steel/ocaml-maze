type maze

val create	: int * int -> maze
val get_width	: maze -> int
val get_height	: maze -> int
val get_cell	: maze -> int * int -> Cell.cell
val solve	: maze -> int -> int -> int -> int -> unit
