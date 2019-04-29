open Graphics
open Gameobj
open Gamedraw

let gameArray = ref (Array.make_matrix 1 1 Empty)

let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

let run () : unit = 
  let drawArray = fun arr -> Array.iter
                        (fun obj -> match obj with
                                    | Empty -> ()
                                    | Wall w -> w#draw
                                    | Box b -> b#draw)
                        arr in
  while true do
    clear_graph ();
    Array.iter drawArray !gameArray ;
    delay 0.05 ;
    synchronize ()
    (* raise Exit *)
  done


let _ =
    window_initialize() ;
    gameArray := Gamemap.generateMap 13 11;
    run ()