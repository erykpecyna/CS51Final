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
  object (this)
    inherit drawable p

    method getSquareCoords = (p#x - rad, p#y - rad) 

    method getArrCoords (objW : int) (objW : int) =
      let (x, y) = this#getSquareCoords in
      x / objW, y / objW

    method draw =
      fill_circle p#x p#y rad
  end

class player (p : point) (rad : int) (w : int) (h : int) =
  object (this)
    inherit moveable p rad as super
    val mutable counter = 0 
		val mutable moving = 0 
		val mutable fin = 0 
    val mutable animJump = 0
    val teleport = 3
			
    method animate =
      if counter = 2 then
        ((if moving = 1 then pos#moveTo fin pos#y
        else pos#moveTo pos#x fin);
        moving <- 0)
      else
        (if moving = 1 then pos#move animJump 0
        else pos#move 0 animJump;
        counter <- counter + 1)

    method moving = moving <> 0
    
    method move (x: int) (y: int) =
			if moving = 0 then
        (let xM = pos#y = y in
        moving <- if xM then 1 else 2;
        fin <- if xM then x else y;
        animJump <- if xM then (fin - pos#x) / 3
                    else (fin - pos#y) / 3 ;
        counter <- 0;
        this#animate)      

    method! draw =
      set_color (rgb 0 255 0) ;
      if moving <> 0 then this#animate;
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
| Bomb of bomb