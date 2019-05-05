open Array ;;
open Gameobj ;;
open Util ;;
module ObjSet = Set.Make (struct
												type t = (int * int)
												let compare = compare
											end) ;;

(* Generates a new map with boxes placed to allow for a proper start for all
	players. Takes arguments for the size of the map. *)
let generateMap (w : int)
								(h : int)
								(screenwidth : int)
								(screenheight : int) =
	let xwidth = screenwidth / w in
	let ywidth = screenheight / h in
	let map = make_matrix w h Empty in
	for x = 0 to (w - 1) do
		for y = 0 to (h - 1) do
		if (x = 0 || x = w - 1 || y = 0 || y = h - 1
				|| (x mod 2 = 0 && y mod 2 = 0)) then
			map.(x).(y) <- Wall (new wall
																(new point (x*xwidth) (y*ywidth))
																xwidth
																ywidth)
		else
		let rand = Random.int 2 in
		if rand = 1 then map.(x).(y) <- Box (new box
																						(new point (x*xwidth) (y*ywidth))
																						xwidth
																						ywidth)
		else map.(x).(y) <- Empty
		done
	done ;
	map.(1).(1) <- Empty ;
	map.(2).(1) <- Empty ;
	map.(1).(2) <- Empty ;
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
		val mutable prev_dir = 0
		val player = new player
													(new point (screenW / mapW)
																		 (screenH / mapH))
													(screenH / mapH / 2)
													(screenW / mapW)
													(screenH / mapH)
		val enemy = new enemy
		                     (new point (screenW) (screenH))
												 (screenH / 2)
												 (screenW)
												 (screenH)


		method tickBombs =
			let run = ref true in
			ObjSet.iter (fun (x, y) ->
										match gameArray.(x).(y) with
										| Bomb b ->
											let Bomb b = gameArray.(x).(y) in
											if b#tick then
												(this#explode x y b)
										| _ -> ())
								bomblist

    method tickExploding =
      ObjSet.iter (fun (x, y) ->
									let Exploding e = gameArray.(x).(y) in
									if e#tick then
										(gameArray.(x).(y) <- Empty;
										explodinglist <- ObjSet.remove (x,y) explodinglist))
								explodinglist

		method explode (xPos : int) (yPos : int) (b : bomb) =
			let explode' x y =
			gameArray.(x).(y) <- Exploding (new exploding
																				(new point (x*objectWidth) (y*objectHeight))
																				objectWidth
																				objectHeight);
			explodinglist <- ObjSet.add (x, y) explodinglist in
			let directionalExplosion x y check =
				match check with
				| Empty -> explode' x y; true
				| Wall _ -> false
				| Box _ ->
          let rand = Random.int 25 in
          if rand = 1 then
            gameArray.(x).(y) <- Powerup (new extrabomb
																				    (new point (x*objectWidth) (y*objectHeight))
																				    objectWidth
																				    objectHeight :> powerup)
          else if rand = 2 then
            gameArray.(x).(y) <- Powerup (new firepower
																				    (new point (x*objectWidth) (y*objectHeight))
																				    objectWidth
																				    objectHeight :> powerup)
					else explode' x y;
          false
				| Bomb b -> this#explode x y b; false
				| Exploding e -> true
				| _ -> true
			in
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

		method movePlayer (dir : char) =
			if not player#moving then
				(if dir = ' ' then
					(if player#bombcount > 0 then
						(player#dropbomb;
						let (x, y) = player#getArrCoords objectWidth objectHeight in
						if gameArray.(x).(y) = Empty then
							(gameArray.(x).(y) <- Bomb (new bomb
																							(new point player#xPos player#yPos)
																							(objectHeight/2));
							bomblist <- ObjSet.add (x,y) bomblist)))
				else
				(let mov = match dir with
				| 'w' -> (0, 1)
				| 'a' -> (-1, 0)
				| 's' -> (0, -1)
				| 'd' -> (1, 0)
				| _ -> (0, 0) in
				let (oldx, oldy) = player#getArrCoords objectWidth objectHeight in
				let (newx, newy) = oldx + fst mov, oldy + snd mov in
				match gameArray.(newx).(newy) with
				| Empty
				| Exploding _ ->
					player#move (newx * objectWidth)
											(newy * objectHeight)
				| Powerup p ->
					(if p#id = 1 then player#addbomb
					else player#addblastradius);
					gameArray.(newx).(newy) <- Empty;
					player#move (newx * objectWidth)
											(newy * objectHeight)
				| _ -> ()))

		method rec moveEnemies (dir : int) (prev : int) =
		  if dir = prev then moveEnemies (Random.int 4) prev
			else (let mov = match dir with
		  | 0 -> (0, 1)
		  | 1 -> (-1, 0)
		  | 2 -> (0, -1)
		  | 3 -> (1, 0)
		  | _ -> (0, 0) in
		  let (oldx, oldy) = enemy#getArrCoords objectWidth objectHeight in
		  let (newx, newy) = oldx + fst mov, oldy + snd mov in
		  match gameArray.(newx).(newy) with
		  | Empty
			| Exploding _ ->
			enemy#move (newx * objectWidth)
								 (newy * objectHeight); moveEnemies (Random.int 5) dir)
			| Powerup p ->
			enemy#move (newx * objectWidth)
								 (newy * objectHeight); moveEnemies (Random.int 5) dir)
			| _ -> ()

		method drawState =
			Array.iter drawArray gameArray ;
			player#draw
			enemy#draw
end
