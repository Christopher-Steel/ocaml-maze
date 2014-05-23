type doorStatus = Open | Close
type door = {mutable door_state: doorStatus}

let create () =
  {door_state = Close};;

let open_door d =
  d.door_state <- Open;;

let close_door d =
  d.door_state <- Close;;

let is_open d =
  d.door_state = Open;;
