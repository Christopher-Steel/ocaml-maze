let get_args () =
  let usage_str = "USAGE: step1 size_X size_Y" in
  let x_int_of_string str =
    try int_of_string str with
      | Failure msg		->
	raise (Invalid_argument (msg ^ " error\n" ^ usage_str))
  in
  if Array.length Sys.argv == 3 then
    let x = x_int_of_string Sys.argv.(1) in
    let y = x_int_of_string Sys.argv.(2) in
    if x <= 0 || y <= 0 then
      raise (Invalid_argument
	       (usage_str ^ "\nValues must be greater than zero"))
    else
      (x, y)
  else
    raise (Invalid_argument usage_str)
;;

let () =
  let mapSize = try get_args () with
    | Invalid_argument msg	->
      (print_endline msg; (0, 0))
    | _				->
      (print_endline "Unknown exception caught in argument parsing"; (0, 0))
  in
  if mapSize <> (0, 0) then
    let m = Maze.create mapSize in
    Maze.solve m 0 0 ((Maze.get_width m)-1) ((Maze.get_height m)-1);
    Displayer.print m;
  else
    exit 1
;;
