open Graphics
open Gameobj
open Gamedraw
open Util


let gameArray = ref (Array.make_matrix 1 1 Empty)
let mapWidth = 15
let mapHeight = 13
let objectWidth = screenWidth / mapWidth
let objectHeight = screenHeight / mapHeight

let player = new player
                  (new point (objectWidth * 3 / 2)
                             (objectHeight * 3 / 2))
                  (objectHeight / 2)

let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

let movePlayer (k : char) =
  if (k = 'd') then player#move



let run () : unit = 
  (* flush_kp was found as a bandaid patch to Ocaml's crappy event handling
     at https://stackoverflow.com/questions/6390631/ocaml-module-graphics-queuing-keypresses
     from user Benoit Fraikin *)
  let flush_kp () = while key_pressed () do
                      let c = read_key ()
                      in ()
                  done in
  let drawArray = fun arr -> Array.iter
                        (fun obj -> match obj with
                                    | Empty -> ()
                                    | Wall w -> w#draw
                                    | Box b -> b#draw)
                        arr in
  while true do
    (* Handle Keyboard input *)
    let inputstatus = wait_next_event [Poll] in
    if (key_pressed ()) then movePlayer inputstatus.key ;
    flush_kp () ;
    (* Clear old frame and draw new one *)
    clear_graph ();
    Array.iter drawArray !gameArray ;
    player#draw ;
    delay 0.05 ;
    synchronize ()
  done


let _ =
    window_initialize() ;
    gameArray := Gamemap.generateMap (mapWidth - 2) (mapHeight - 2);
    run ()