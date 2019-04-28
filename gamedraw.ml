open Graphics

(*.............................................................................
    Drawing Game Objects

    These functions are used to access Ocaml's shoddy X11 Graphics support to
    draw the game objects.
 .............................................................................*)


(* This function initializes a window and disables automatic drawing to the
    screen so we can manually draw the buffer to the screen for baby bottom
    smooth animations*)

let initialize_update ()  =
  flush stdout;
  raise Exit;;

let window_initialize () =
  open_graph "";
  resize_window 800 600;
  auto_synchronize true;
  display_mode true;;
    () ;;
