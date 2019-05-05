open Graphics
open Gameobj
open Util
open GameState

(* To be used for animation *)
let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

(* Used to open a new game window *)
let window_initialize () =
  open_graph "";
  resize_window 1200 897;
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
  state#makeEnemies 3;

  (* Open a graphics window and start the game loop *)
  window_initialize();
  set_window_title "Ocaml Bomberman";

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
    delay 0.05 ;
    synchronize ()
  done ;

  (* Game Over Screen *)
  delay 0.5;
  clear_graph ();
  set_color black;
  fill_rect 0 0 1200 900; 
  set_color white;
  moveto 600 450;
  if state#won then
    draw_string "Game Won"
  else draw_string "Game Over";
  synchronize ();
  delay 1.5

let _ =
  let newState = new state 15 13 1200 897 in
  run newState