open Array ;;
open Gameobj ;;
open Util ;;

val generateMap : int -> int -> int -> int -> unit

val drawArray

class state : int -> int -> int -> int -> object

  method alive : bool

  method won : bool

  method tickBombs : unit

  method tickExploding : unit

  method explode : int -> int -> bomb : unit

  method movePlayer : char -> unit

  method makeEnemies : int -> unit

  method moveEnemies : unit

  method drawState : unit 

end
