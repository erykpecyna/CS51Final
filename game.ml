open Graphics ;;
open Gameobj ;;
open Util ;;
open GameState ;;
open Config ;;

(*.............................................................................
  Game 
  The functions in this file all work at the highest level of the BomberCaml
  game. They control things like framerate and the start/end of the game.
.............................................................................*)

(* To be used for animation *)
let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

(* Used to open a new game window *)
let window_initialize () =
  open_graph "";
  resize_window sCREENWIDTH sCREENHEIGHT;
  auto_synchronize false;
  display_mode false;;

(* Takes a game state and runs it until the game ends *)
let run state : unit =
  (* flush_kp was found as a bandaid patch to Ocaml's spotty event handling
     at https://stackoverflow.com/questions/6390631/ocaml-module-graphics-queuing-keypresses
     from user Benoit Fraikin *)
  let flush_kp () = while key_pressed () do
                      let _ = read_key ()
                      in ()
                  done in

  (* Make some Enemies *)
  state#makeEnemies nUMENEMIES;

  (* Open a graphics window and start the game loop *)
  window_initialize();
  set_window_title "BomberCaml";

  while state#alive && not state#won do
    (* Handle Keyboard input *)
    let inputstatus = wait_next_event [Poll] in
    if (key_pressed ()) then
      if (inputstatus.key = 'w' || inputstatus.key = 'a'
          || inputstatus.key = 's' || inputstatus.key = 'd'
          || inputstatus.key = ' ') then
        state#movePlayer inputstatus.key ;
    flush_kp () ;

    (* Clear old frame and draw new one with a delay to simulate
      a refresh rate of 20 frames per second although it will be slightly
      less due to runtime *)
    clear_graph ();
    state#drawState;
    delay uPDATEDELAY ;
    synchronize ()
  done ;

  (* Game Over Screen *)
  delay eNDDELAY;
  clear_graph ();
  set_color black;
  fill_rect 0 0 sCREENWIDTH sCREENHEIGHT;
  set_color white;
  moveto cENTERX cENTERY;
  if state#won then
    draw_string "Game Won"
  else draw_string "Game Over";
  synchronize ();
  delay cLOSEDDELAY

let _ =
  let newState = new state mAPWIDTH mAPHEIGHT in
  run newState
