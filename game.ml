open Graphics
open Gameobj
open Gamedraw

let _ =
    open_graph "";
    resize_window 800 600;
    auto_synchronize false;
    display_mode false;
    let test = Gamemap.generateMap 13 11 in
    let f = fun f -> Array.iter
                        (fun obj -> match obj with
                                    | Empty -> ()
                                    | Wall w -> w#draw
                                    | Box b -> b#draw)
                        f in
    Array.iter f test ;;