open Graphics ;;
open Util ;;

(*.............................................................................
  Graphical Objects 

  This class provides a basis for every object in the game. The game objects
  will necessarily have more functionality, but the drawable object will
  necessitate their ability to be drawn on the screen
.............................................................................*)

class virtual drawable (p : point) =
  object (this)
    val pos : point = p
    method virtual draw : unit
  end 

class bomb (m : point) =
  object
    inherit drawable m
    (* val radius : int = r *)
    
    method draw = ()
  end 

(*.............................................................................
  Game Object Types 

  These types differentiate between the types objects that may need to be
  created
.............................................................................*)
type gameobject =
| Empty
| Wall
| Box ;;