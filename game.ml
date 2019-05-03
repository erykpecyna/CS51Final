open Graphics
open Gameobj
open Gamedraw
open Util
open GameState

let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

(* let movePlayer (k : char) =
  if (k = 'd') then player#move *)



let run state : unit = 
  (* flush_kp was found as a bandaid patch to Ocaml's crappy event handling
     at https://stackoverflow.com/questions/6390631/ocaml-module-graphics-queuing-keypresses
     from user Benoit Fraikin *)
  let flush_kp () = while key_pressed () do
                      let c = read_key ()
                      in ()
                  done in
  window_initialize() ;
  while true do
    (* Handle Keyboard input *)
    let inputstatus = wait_next_event [Poll] in
    if (key_pressed ()) then
      if (inputstatus.key = 'w' || inputstatus.key = 'a'
          || inputstatus.key = 's' || inputstatus.key = 'd') then
        state#movePlayer inputstatus.key ;
    flush_kp () ;
    (* Clear old frame and draw new one *)
    clear_graph ();
    state#drawState;
    delay 0.05 ;
    synchronize ()
  done

let _ =
  let newState = new state 15 13 800 600 in
  run newState