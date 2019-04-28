open Graphics

(*.............................................................................
    Drawing Game Objects

    These functions are used to access Ocaml's shoddy X11 Graphics support to
    draw the game objects.
 .............................................................................*)

module G = Graphics ;;

(* This function initializes a window and disables automatic drawing to the
    screen so we can manually draw the buffer to the screen for baby bottom
    smooth animations*)
let window_initialize () =
  G.open_graph "";
  G.resize_window 800 600;
  G.auto_synchronize true;
  G.display_mode true;;

let render_graphics =
    () ;;

let close_window =
    ignore (G.read_key ()) ;;

let _ = window_initialize () ;;