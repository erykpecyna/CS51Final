open Graphics
open Gameobj
open Util
open GameState

let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

let window_initialize () =
  open_graph "";
  resize_window 1200 897;
  auto_synchronize false;
  display_mode false;;

let run state : unit = 
  (* flush_kp was found as a bandaid patch to Ocaml's spotty event handling
     at https://stackoverflow.com/questions/6390631/ocaml-module-graphics-queuing-keypresses
     from user Benoit Fraikin *)
  let flush_kp () = while key_pressed () do
                      let _ = read_key ()
                      in ()
                  done in
  
  (* Open a graphics window and start the game loop *)
  window_initialize() ;
  while true do
    (* Handle Keyboard input *)
    let inputstatus = wait_next_event [Poll] in
    if (key_pressed ()) then
      if (inputstatus.key = 'w' || inputstatus.key = 'a'
          || inputstatus.key = 's' || inputstatus.key = 'd'
          || inputstatus.key = ' ') then
        state#movePlayer inputstatus.key ;
    flush_kp () ;

    (* Handle Game Object Movement/action *)
    state#tickExploding;
    state#tickBombs;

    (* Clear old frame and draw new one *)
    clear_graph ();
    state#drawState;
    delay 0.05 ;
    synchronize ()
  done

let _ =
  let newState = new state 15 13 1200 897 in
  run newState