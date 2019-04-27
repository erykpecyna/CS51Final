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
    inherit drawable
    val pos : point = m
    val radius : int = r
    val textcolor : color = textcol
    val linewidth : int = linewidth
                       
    method draw =
      let (x, y) as p = anchor#round in
      set_line_width linewidth;
      set_color background;
      fill_circle x y radius;
      set_color color;
      draw_circle x y radius;
      set_color textcolor;
      draw_text_centered label p
  end

