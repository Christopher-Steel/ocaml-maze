open Sdlevent
open Sdlkey

let rec wait_quit () =
  match wait_event () with
    | QUIT				-> ()
    | KEYDOWN {keysym = KEY_ESCAPE}	-> ()
    | _					-> wait_quit ()

let win_init w h name color =
  let win_open w h color =
    let screen = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    Sdlvideo.fill_rect screen (Sdlvideo.map_RGB screen color);
    Sdlvideo.flip screen;
    screen
  in
  (
    try
      (
	Sdl.init [`VIDEO];
	at_exit Sdl.quit;
	Sdlwm.set_caption ~title:name ~icon:"";
	win_open w h color
      )
    with
	Sdl.SDL_init_exception(msg) ->
	  (
	    print_endline "Error: Can't init SDL library";
	    exit (1)
	  )
  )

let rec draw_wall screen x y width heigth color =
  let rec draw_line screen x y length =
    if length > 0 then
      (
	Sdlvideo.lock screen;
	Sdlvideo.put_pixel_color screen ~x:x ~y:y color;
	Sdlvideo.unlock screen;
	draw_line screen (x + 1) y (length - 1)
      )
  in
  if heigth > 0 then
    (
      draw_line screen x y width;
      draw_wall screen x (y + 1) width (heigth - 1) color
    )

let rec draw_case screen m line col coef =
  let draw_path () =
    let cell_color = (Cell.get_color (Maze.get_cell m (col, line))) in
    let color_south =
      if line < ((Maze.get_height m)-1) then
	Cell.get_color (Maze.get_cell m (col, (line+1)))
      else
	-1
    in
    let color_east =
      if col < ((Maze.get_width m)-1) then
	Cell.get_color (Maze.get_cell m ((col+1), line))
      else
	-1
    in
    let color1 = Sdlvideo.cyan in
    let color2 = (50, 50, 150) in
    (
      if cell_color = 1 then
	(
	  draw_wall screen ((col*2*coef)+coef) ((line*2*coef)+coef) coef coef color1;
	  if color_south > 0 then
	    draw_wall screen ((col*2*coef)+coef) ((line*2*coef)+2*coef) coef coef color1;
	  if color_east > 0 then
	    draw_wall screen ((col*2*coef)+2*coef) ((line*2*coef)+coef) coef coef color1;
	)
      else if cell_color = 2 then
	(
	  draw_wall screen ((col*2*coef)+coef) ((line*2*coef)+coef) coef coef color2;
	  if color_south = 2 then
	    draw_wall screen ((col*2*coef)+coef) ((line*2*coef)+2*coef) coef coef color2
	  else if color_south = 1 then
	    draw_wall screen ((col*2*coef)+coef) ((line*2*coef)+2*coef) coef coef color1;
	  if color_east = 2 then
	    draw_wall screen ((col*2*coef)+2*coef) ((line*2*coef)+coef) coef coef color2
	  else if color_east = 1 then
	    draw_wall screen ((col*2*coef)+2*coef) ((line*2*coef)+coef) coef coef color1
	)
    )
  in
  let color = Sdlvideo.black in
  if col != (Maze.get_width m) then
    (
      let cell = Maze.get_cell m (col, line) in
      (
	if col = 0 then
	  draw_wall screen 0 ((line*2*coef)+coef) coef (coef*2) color;
	draw_path ();
	if not (Cell.is_door_open cell Cell.East) then
	  draw_wall screen ((col+1)*2*coef) (line*2*coef) coef (coef*3) color;
	if not (Cell.is_door_open cell Cell.South) then
	  draw_wall screen (col*2*coef) ((line+1)*2*coef) (coef*3) coef color;
	draw_case screen m line (col+1) coef
      )
    )

let rec draw_line_lab screen m line coef=
  if line = (Maze.get_height m) then
      draw_case screen m line (Maze.get_width m) coef
  else
    (
      draw_case screen m line 0 coef;
      draw_line_lab screen m (line + 1) coef
    )

let draw_lab screen m coef =
  (
    draw_wall screen 0 0 (((Maze.get_width m)*2+1)*coef) coef Sdlvideo.black;
    draw_line_lab screen m 0 coef
  )

let print m =
  let background_color = Sdlvideo.white in
  let width = (Maze.get_width m) in
  let height = (Maze.get_height m) in
  let coef =
    if (width <= 22 && height <= 22) then 20
    else
      if (width < height) then
	900 / ((height * 2) + 1)
      else
	900 / ((width * 2) + 1)
  in
  let screen =
    win_init ((width*2*coef)+coef) ((height*2*coef)+coef) "A-maze-ing" background_color
  in
  (
    draw_lab screen m coef;
    Sdlvideo.flip screen;
    wait_quit ()
  )
