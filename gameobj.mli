open Graphics ;;
open Util ;;

class virtual drawable : point -> object
  method virtual draw : unit
end

class wall : point -> int -> int -> object
  inherit drawable

  method draw : unit
 end

class box : point -> int -> int -> object
  inherit drawable

  method draw : unit
end

class bomb : point -> int -> object
  inherit drawable

  method tick : unit

  method draw : unit
 end

class exploding : point -> int -> int -> object
  inherit drawable

  method tick : unit

  method draw : unit
 end

class powerup : point -> int -> int -> object
  inherit drawable

  method id : int

  method draw : unit
 end

class extrabomb : point -> int -> int -> object
  inherit powerup

  method draw : unit
 end

class firepower : point -> int -> int -> object
  inherit powerup

  method draw : unit
 end

class moveable : point -> int -> int -> int -> object
  inherit drawable

  method animate : unit

  method moving : bool

  method move : int -> int -> unit

  method getArrCoords : (int * int)

  method xPos : int

  method yPos : int

  method draw : unit
 end

class player : point -> int -> int -> int -> object
  inherit moveable

  method blastradius : int

  method addblastradius : unit

  method bombcount : int

  method dropbomb : unit

  method addbomb : unit

  method move : int -> int -> unit
 end

class enemy : point -> int -> int -> int -> object
  inherit moveable

  method getdir : unit

  method draw : unit
 end

 type gameobject =
 | Empty
 | Wall of wall
 | Box of box
 | Bomb of bomb
 | Exploding of exploding
 | Powerup of powerup
