type coord =
  | North
  | East
  | South
  | West

type cell =
    {
      mutable color : int;
      n : Door.door;
      e : Door.door;
      s : Door.door;
      w : Door.door;
    }
;;

exception No_closed_doors;;
exception Invalid_door_dir;;
Random.self_init ()

let nb_doors = 4

let create color =
  {
    color = color;
    n = Door.create ();
    e = Door.create ();
    s = Door.create ();
    w = Door.create ();
  }

let door_of_coord cl = function
  | North	-> cl.n
  | East	-> cl.e
  | South	-> cl.s
  | West	-> cl.w

let door_of_dir cl = function
  | 0 	-> cl.n
  | 1 	-> cl.e
  | 2 	-> cl.s
  | 3 	-> cl.w
  | _ -> raise Invalid_door_dir

let open_door cl dir =
  Door.open_door (door_of_coord cl dir)

let is_door_open cl dir =
  Door.is_open (door_of_coord cl dir)

let rand_closed_door cl =
  let coord_of_int = function
    | 0	-> North
    | 1	-> East
    | 2	-> South
    | 3	-> West
    | _	-> raise Invalid_door_dir
  in
  let rec try_rand_door tried =
    if (List.length tried >= nb_doors) then
      raise No_closed_doors
    else
      let i = Random.int (nb_doors - (List.length tried)) in
      let rec next_attempt nb =
	if (not (List.mem nb tried)) || (nb = (nb_doors - 1)) then
	  nb
	else
	  next_attempt (nb + 1)
      in
      let current_door_nb = next_attempt i in
      let current_door = door_of_dir cl current_door_nb in
      if not (Door.is_open current_door) then
	coord_of_int current_door_nb
      else
	try_rand_door (i::tried)
  in
  try_rand_door []

let iter fn cl =
  fn North;
  fn East;
  fn South;
  fn West

let opposite_door = function
  | North	-> South
  | East	-> West
  | South	-> North
  | West	-> East

let get_color cl =
  cl.color

let set_color cl color =
  cl.color <- color
