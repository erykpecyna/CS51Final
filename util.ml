(*.............................................................................
    Utility Objects 

    These classes will be used to compose game objects. They exist to simplify
    the code and keep some basic functions required for all objects in one
    place for easy debugging.
.............................................................................*)

class point (x0 : int) (y0 : int) =   
object (this)
  val mutable x = x0
  val mutable y = y0

  method x : int = x
  method y : int = y

  method pos : int * int = x, y
  
  method move (xdist : int) (ydist : int) : unit =
    x <- x + xdist;
    y <- y + ydist 
end