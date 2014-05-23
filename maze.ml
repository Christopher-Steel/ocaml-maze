type maze =
    {
      width: int;
      height: int;
      cells: Cell.cell array array;
    }
;;

exception No_neighbour

let create (maze_width, maze_height) =
  let new_maze =
    {
      width = maze_width;
      height = maze_height;
      cells =
	Array.init maze_height (fun y ->
	  Array.init maze_width (fun x ->
	    Cell.create (y * maze_width + x)
	  )
	)
    }
  in
  let cell_neighbour x y dir =
    let neighbour_of_coord = function
      | Cell.North	-> (x, y - 1)
      | Cell.East	-> (x + 1, y)
      | Cell.South	-> (x, y + 1)
      | Cell.West	-> (x - 1, y)
    in
    let ngh = neighbour_of_coord dir in
    let	ngh_x, ngh_y = ngh in
    if ngh_x < 0 || ngh_x >= maze_width
      || ngh_y < 0 || ngh_y >= maze_height
    then
      raise No_neighbour
    else
      ngh
  in
  let rec set_color x y color =
    let cl = new_maze.cells.(y).(x) in
    let old_color = Cell.get_color cl in
    begin
      Cell.set_color cl color;
      Cell.iter (fun dir ->
	let ngh_x, ngh_y =
	  try
	    cell_neighbour x y dir
	  with
	    | No_neighbour	-> (-1, -1)
	in
	if ngh_x <> -1
	  && (Cell.get_color new_maze.cells.(ngh_y).(ngh_x)) = old_color
	then
	  set_color ngh_x ngh_y color
      ) cl;
    end
  in
  let connect_cells cl_x cl_y ngh_x ngh_y dir =
    let cl = new_maze.cells.(cl_y).(cl_x) in
    let ngh = new_maze.cells.(ngh_y).(ngh_x) in
    if (Cell.get_color cl) = (Cell.get_color ngh) then
      false
    else (
      set_color ngh_x ngh_y (Cell.get_color cl);
      Cell.open_door cl dir;
      Cell.open_door ngh (Cell.opposite_door dir);
      true
    )
  in
  let rec pick_a_door () =
    let rand_x = Random.int maze_width in
    let rand_y = Random.int maze_height in
    try
      let cl = new_maze.cells.(rand_y).(rand_x) in
      let dir = Cell.rand_closed_door cl in
      let ngh_x, ngh_y = cell_neighbour rand_x rand_y dir in
      connect_cells rand_x rand_y ngh_x ngh_y dir
    with
      | Cell.No_closed_doors | No_neighbour	-> pick_a_door ()
  in
  let rec generate_perfect_maze = function
    | 1	-> ()
    | n ->
      let colors =
	if pick_a_door () then
	  n - 1
	else
	  n
      in
      generate_perfect_maze colors
  in
  begin
    generate_perfect_maze (maze_width * maze_height);
    new_maze
  end

let get_width m =
  m.width

let get_height m =
  m.height

let get_cell m (x, y) =
  m.cells.(y).(x)

let solve m sx sy gx gy =
  let starting_check () =
    if sx < 0 || sy < 0 || sx >= m.width || sy >= m.height ||
      gx < 0 || gy < 0 || gx >= m.width || gy >= m.height then
      invalid_arg "Invalid starting or destination position"
  in
  let reset_color cells =
    Array.iter (fun arr ->
      Array.iter (fun cl ->
	Cell.set_color cl 0
      ) arr
    ) cells
  in
  let rec go_through sy sx gy gx coord =
    let cell = get_cell m (sx, sy) in
    begin
      Cell.set_color cell 2;
      if sx = gx && sy = gy then true
      else
	begin
	  if (((Cell.opposite_door coord) != Cell.North) &&
	    (Cell.is_door_open cell Cell.North) &&
	    (go_through (sy - 1) sx gx gy Cell.North))
	  then true
	  else if (((Cell.opposite_door coord) != Cell.South) &&
	    (Cell.is_door_open cell Cell.South) &&
	    (go_through (sy + 1) sx gx gy Cell.South))
	  then true
	  else if (((Cell.opposite_door coord) != Cell.East) &&
	    (Cell.is_door_open cell Cell.East) &&
	    (go_through sy (sx + 1) gx gy Cell.East))
	  then true
	  else if (((Cell.opposite_door coord) != Cell.West) &&
	    (Cell.is_door_open cell Cell.West) &&
	    (go_through sy (sx - 1) gx gy Cell.West))
	  then true
	  else
	    begin
	      Cell.set_color cell 1;
	      false
	    end
	end
    end
  in
  begin
    starting_check ();
    reset_color m.cells;
    ignore (go_through sy sx gy gx Cell.North);
    ignore (go_through sy sx gy gx Cell.East);
    ignore (go_through sy sx gy gx Cell.South);
    ignore (go_through sy sx gy gx Cell.West);
    Cell.set_color m.cells.(sy).(sx) 2;
  end
