open Graphics ;;
open Util ;;

(*.............................................................................
  Graphical Objects 
  The drawable class provides a basis for every object in the game.
  Other game objects necessarily have more functionality, but the drawable
  object will necessitate their ability to be drawn on the screen
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

    val mutable timer = 30

    method tick = timer <- timer - 1; timer <= 0

    method draw =
      set_color (rgb 0 0 0) ;
      fill_circle (p#x + rad) (p#y + rad) rad
  end

class exploding (p : point) (w : int) (h : int) =
  object
    inherit drawable p

    val mutable timer = 10

    method tick = timer <- timer - 1; timer <= 0
    
    method draw =
      set_color (rgb 255 247 0) ;
      fill_rect p#x p#y w h
  end

class powerup (p : point) (w : int) (h: int) =
  object
    inherit drawable p
    val id = 0

    method id = id

    method draw =
      set_color (rgb 0 0 0) ;
      fill_rect p#x p#y w h
  end 

class extrabomb (p: point) (w: int) (h: int) =
  object 
    inherit powerup p w h as super
    val! id = 1

    method! draw =
      set_color (rgb 255 255 0) ;
      fill_rect p#x p#y w h ;
      set_color (rgb 0 0 0) ;
      let rad = h/2 in 
      fill_circle (p#x + rad) (p#y + rad) rad 
  end

class firepower (p: point) (w: int) (h: int) =
  object 
    inherit powerup p w h as super
    val! id = 2

    method! draw =
      set_color (rgb 255 255 0) ;
      fill_rect p#x p#y w h ;
      set_color (rgb 255 0 0) ;
      let rad = h/2 in 
      fill_circle (p#x + rad) (p#y + rad) rad 
  end

(* Character Types *)

class moveable (p : point) (rad : int) (w : int) (h : int) =
  object (this)
    inherit drawable p
    val mutable counter = 0 
    val mutable dirmoving = 0 
    val mutable fin = 0 
    val mutable animJump = 0
    val teleport = 3

    (* 3-step animation for movement *)  
    method animate =
      if counter = 2 then
        ((if dirmoving = 1 then pos#moveTo fin pos#y
        else pos#moveTo pos#x fin);
        dirmoving <- 0)
      else
        (if dirmoving = 1 then pos#move animJump 0
        else pos#move 0 animJump;
        counter <- counter + 1)

    method moving = dirmoving <> 0
    
    (* Sets up the start of the 3-step animation *)
    method move (x: int) (y: int) =
      if dirmoving = 0 then
        (let xM = pos#y = y in
        dirmoving <- if xM then 1 else 2;
        fin <- if xM then x else y;
        animJump <- if xM then (fin - pos#x) / 3
                    else (fin - pos#y) / 3 ;
        counter <- 0)     

    method getArrCoords =
      p#x / w, p#y / h

    method xPos = pos#x
    method yPos = pos#y

    method draw =
      set_color (rgb 0 255 0) ;
      if dirmoving <> 0 then this#animate ;
      fill_circle (p#x + rad) (p#y + rad) rad
  end

class player (p : point) (rad : int) (w : int) (h : int) =
  object (this)
    inherit moveable p rad w h as super

    val mutable bombcount = 1
    val mutable blastradius = 1

    method blastradius = blastradius
    method addblastradius = blastradius <- blastradius + 1
    method bombcount = bombcount
    method dropbomb = bombcount <- bombcount - 1
    method addbomb = bombcount <- bombcount + 1

    (* Skip first frame of player animation for snappier control response *)
    method! move (x : int) (y : int) =
      super#move x y;
      super#animate;
  end

class enemy (p : point) (rad : int) (w : int) (h : int) =
  object (this)
    inherit moveable p rad w h
    val mutable lastdir = 'D'
    val mutable even = false
    val dirlist = ['L'; 'R'; 'U'; 'D']

    (* Finds the next movement direction of the enemy *)
    method getdir =
      let getmatch x =
        match x with
        | 'L' -> 'R'
        | 'R' -> 'L'
        | 'U' -> 'D'
        | 'D' -> 'U'
        | _ -> ' ' in
        if even then
          (even <- not even; lastdir)
        else
        (even <- not even; 
        let rand = Random.int 3 in
        let dir = List.nth (List.filter (fun x -> x <> getmatch lastdir)
                              dirlist) rand in
        lastdir <- dir; dir)

    method! draw =
      set_color (rgb 255 0 0);
      if dirmoving <> 0 then this#animate ;
      fill_circle (p#x + rad) (p#y + rad) rad
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
| Exploding of exploding
| Powerup of powerup