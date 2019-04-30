open Graphics ;;
open Util ;;

let screenWidth = 800
let screenHeight = 600
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

class wall (p : point) (w : int) (h : int) =
  object
    inherit drawable p

    method draw =
      set_color (rgb 128 128 128) ;
      fill_rect p#x p#y w h
  end

class box (p : point) (w : int) (h : int) =
  object
    inherit drawable p

    method draw =
      set_color (rgb 139 69 19) ;
      fill_rect p#x p#y w h
  end

class bomb (p : point) (rad : int) =
  object
    inherit drawable p

    method draw =
      set_color (rgb 0 0 0) ;
      fill_circle p#x p#y rad
  end

(* Character Types *)

class moveable (p : point) (rad : int) =
  object
    inherit drawable p

    method draw =
      fill_circle p#x p#y rad
  end

class player (p : point) (rad : int) =
  object
    inherit moveable p rad as super

    method move =
      pos#move 1 0

    method! draw =
      set_color (rgb 0 255 0) ;
      super#draw 
  end

class enemy (p : point) (rad : int) =
  object
    inherit moveable p rad as super

    method! draw =
      set_color (rgb 255 0 0) ;
      super#draw 
  end
(*.............................................................................
  Game Object Types 

  These types differentiate between the types objects that may need to be
  created
.............................................................................*)
type gameobject =
| Empty
| Wall of wall
| Box of box
| Player of player
| Bomb of bomb