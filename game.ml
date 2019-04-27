open Gameobj ;;
open Gamedraw ;;
module G = Graphics ;;
let _ =
    let test = Gamemap.generateMap 13 11 in
    let f = fun f -> Array.iter
                        (fun obj -> match obj with
                                    | Empty -> ()
                                    | Wall w -> w#draw
                                    | Box b -> b#draw)
                        f in
    G.open_graph "";
    G.resize_window 800 600;
    G.auto_synchronize false;
    G.display_mode false;
    Array.iter f test ;;
    
