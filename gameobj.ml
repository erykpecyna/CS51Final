open Graphics ;;
open Util ;;

(*.............................................................................
  Graphical Objects 

  This class provides a basis for every object in the game. The game objects
  will necessarily have more functionality, but the drawable object will
  necessitate their ability to be drawn on the screen
.............................................................................*)

class virtual drawable (p : point)
                       (w : int)
                       (h : int) =
  object (this)
    val pos : point = p
    val width : int = w
    val height : int = h
    method virtual draw : unit
  end 

class wall (p : point)
           (w : int)
           (h : int) =
  object
    inherit drawable p w h

    method draw =
      set_color cyan ;
      fill_rect p#x p#y w h
  end

class box (p : point)
           (w : int)
           (h : int) =
  object
    inherit drawable p w h

    method draw =
      set_color green ;
      fill_rect p#x p#y w h
  end
(*.............................................................................
  Game Object Types 

  These types differentiate between the types objects that may need to be
  created
.............................................................................*)
type gameobject =
| Empty
| Wall of wall
| Box of box;;