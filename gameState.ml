open Array ;;
open Gameobj ;;
open Util ;;

(*.............................................................................
  Game State
  This file contains the state of the map and the necessary methods needed to
  alter the state of the map. It also contains the generateMap function that
  creates the starting map.
.............................................................................*)

module ObjSet =	Set.Make (struct
                            type t = (int * int)
                            let compare = compare
                          end) ;;

(* Generates a new map with boxes placed to allow for a proper start for all
  players. Takes arguments for the size of the map. *)
let generateMap (w : int) (* width of game map *)
                (h : int) (* height of game map *)
                (screenwidth : int)
                (screenheight : int) =
  let xwidth = screenwidth / w in
  let ywidth = screenheight / h in
  let map = make_matrix w h Empty in
  for x = 0 to (w - 1) do
    for y = 0 to (h - 1) do
      if (x = 0 || x = w - 1 || y = 0 || y = h - 1 ||
          (x mod 2 = 0 && y mod 2 = 0)) then
        map.(x).(y) <- Wall (new wall (new point (x*xwidth) (y*ywidth))
                                      xwidth
                                      ywidth)
      else
      let rand = Random.self_init (); Random.int 10 in
      if rand < 7 then
        map.(x).(y) <- Box (new box (new point (x*xwidth) (y*ywidth))
                                    xwidth
                                    ywidth)
      else map.(x).(y) <- Empty
    done
  done ;
  map.(1).(1) <- Empty ;
  map.(2).(1) <- Empty ;
  map.(1).(2) <- Empty ;
  map.(3).(1) <- Empty ;
  map.(1).(3) <- Empty ;
  map ;;

let drawArray =
  fun arr -> Array.iter (fun obj -> match obj with
                                    | Empty -> ()
                                    | Box b -> b#draw
                                    | Wall w -> w#draw
                                    | Bomb b -> b#draw
                                    | Exploding e -> e#draw
                                    | Powerup p -> p#draw)
                        arr ;;

class state (mapW : int)
            (mapH : int)
            (screenW : int)
            (screenH : int) =
  object (this)
    val objectWidth = screenW / mapW
    val objectHeight = screenH / mapH
    val gameArray = generateMap mapW mapH screenW screenH
    val mutable bomblist = ObjSet.empty
    val mutable explodinglist = ObjSet.empty
    val mutable moveEnemies = 0
    val mutable alive = true
    val mutable won = false
    val player = new player
                          (new point (screenW / mapW)
                                     (screenH / mapH))
                          (screenH / mapH / 2)
                          (screenW / mapW)
                          (screenH / mapH)
    val mutable enemylist = []

    method alive = alive
    method won = won

    (* Decrease timer on dropped bombs *)
    method tickBombs =
      ObjSet.iter (fun (x, y) ->
                    match gameArray.(x).(y) with
                    | Bomb b ->
                      if b#tick then
                        (this#explode x y b)
                    | _ -> bomblist <- ObjSet.remove (x, y) bomblist)
                  bomblist

    (* Decrease timer on exploding tiles *)
    method tickExploding =
      ObjSet.iter (fun (x, y) ->
                    match gameArray.(x).(y) with
                    | Exploding e ->
                      if e#tick then
                        (gameArray.(x).(y) <- Empty;
                        explodinglist <- ObjSet.remove (x,y) explodinglist)
                    | _ -> explodinglist <- ObjSet.remove (x,y) explodinglist)
                  explodinglist

    (* Blow up a bomb whose "fuse" has run out *)
    method explode (xPos : int) (yPos : int) (b : bomb) =
      (* Turn a tile into an exploding tile *)
      let explode' x y =
        if player#getArrCoords = (x, y) then alive <- false ;
        gameArray.(x).(y) <-
          Exploding (new exploding
                    (new point (x*objectWidth) (y*objectHeight))
                    objectWidth
                    objectHeight);
        explodinglist <- ObjSet.add (x, y) explodinglist in
      (* Blow up a tile while checking if explosion should continue *)
      let directionalExplosion x y check =
        match check with
        | Empty -> explode' x y; true
        | Wall _ -> false
        | Box _ ->
          let rand = Random.int 10 in
          if rand = 1 then
            gameArray.(x).(y) <-
              Powerup (new extrabomb (new point (x*objectWidth) (y*objectHeight))
                                      objectWidth
                                      objectHeight)
          else if rand = 2 then
            gameArray.(x).(y) <-
              Powerup (new firepower (new point (x*objectWidth) (y*objectHeight))
                                      objectWidth
                                      objectHeight)
          else explode' x y;
          false
        | Bomb b -> this#explode x y b; false
        | Exploding e -> true
        | _ -> true
      in
      (* Blow up bomb and tiles within effective radius *)
      player#addbomb;
      bomblist <- ObjSet.remove (xPos, yPos) bomblist;
      explode' xPos yPos ;
      let down = ref true in
      let up = ref true in
      let left = ref true in
      let right = ref true in
      for i = 1 to player#blastradius do
        if !down then
          (let check = gameArray.(xPos).(yPos - i) in
          down := directionalExplosion xPos (yPos - i) check) ;
        if !up then
          (let check = gameArray.(xPos).(yPos + i) in
          up := directionalExplosion xPos (yPos + i) check) ;
        if !left then
          (let check = gameArray.(xPos - i).(yPos) in
          left := directionalExplosion (xPos - i) yPos check);
        if !right then
          (let check = gameArray.(xPos + i).(yPos) in
          right := directionalExplosion (xPos + i) yPos check)
        done

    (* Handle player movement and bomb drops *)
    method movePlayer (dir : char) =
      if not player#moving then
        (if dir = ' ' then
          (if player#bombcount > 0 then
            (player#dropbomb;
            let (x, y) = player#getArrCoords in
            gameArray.(x).(y) <- Bomb (new bomb
                                            (new point player#xPos player#yPos)
                                            (objectHeight/2));
            bomblist <- ObjSet.add (x,y) bomblist))
        else
        (let mov = match dir with
        | 'w' -> (0, 1)
        | 'a' -> (-1, 0)
        | 's' -> (0, -1)
        | 'd' -> (1, 0)
        | _ -> (0, 0) in
        let (oldx, oldy) = player#getArrCoords in
        let (newx, newy) = oldx + fst mov, oldy + snd mov in
        match gameArray.(newx).(newy), gameArray.(oldx).(oldy) with
        | Empty, _ ->
          player#move (newx * objectWidth)
                      (newy * objectHeight)
        | Exploding _, _
        | _, Exploding _ ->
          alive <- false;
          player#move (newx * objectWidth)
                      (newy * objectHeight)
        | Powerup p, _ ->
          (if p#id = 1 then player#addbomb
          else player#addblastradius);
          gameArray.(newx).(newy) <- Empty;
          player#move (newx * objectWidth)
                      (newy * objectHeight)
        | _, _ -> ()))

    method makeEnemies (num : int) =
      for _ = 1 to num do
        enemylist <-
          new enemy (new point ((mapW - 2) * objectWidth)
                                ((mapH - 2) * objectHeight))
                    (objectHeight / 2)
                    objectWidth
                    objectHeight
          :: enemylist
      done

    (* Move all enemies on map *)
    method moveEnemies =
      let f = fun i enemy ->
        let mov = match enemy#getdir with
        | 'L' -> (-1, 0)
        | 'R' -> (1, 0)
        | 'U' -> (0, 1)
        | 'D' -> (0, -1)
        | _ -> (0, 0) in
        let (oldx, oldy) = enemy#getArrCoords in
        let (newx, newy) = oldx + fst mov, oldy + snd mov in
        match gameArray.(newx).(newy), gameArray.(oldx).(oldy) with
        | Empty, _
        | Powerup _, _
        | Box _, _
        | Bomb _, _ -> enemy#move (newx * objectWidth)
                                (newy * objectHeight)
        | Exploding _, _
        | _, Exploding _ ->
          enemy#move (newx * objectWidth)
                      (newy * objectHeight);
          enemy#draw;
          enemy#draw;
          let check = List.nth enemylist i in
          enemylist <- List.filter (fun x -> x <> check) enemylist
        | _, _ -> ()
      in
      (* Slow down enemy movement *)
      if moveEnemies = 3 then
        (List.iteri f enemylist;
        moveEnemies <- 0)
      else
      moveEnemies <- moveEnemies + 1

    method drawState =
      (* Handle Game Object Movement/action *)
      this#tickExploding;
      this#tickBombs;
      this#moveEnemies;

      (* Draw game map and characters *)
      Array.iter drawArray gameArray ;
      List.iter (fun enemy ->
                    if enemy#getArrCoords = player#getArrCoords then
                      alive <- false;
                      enemy#draw) enemylist ;
      player#draw;

      (* Check for win *)
      won <- List.length enemylist = 0
  end
