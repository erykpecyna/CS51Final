open Array ;;
open Gameobj ;;
open Util ;;

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
						   | Bomb b -> b#draw)
						   arr ;;

class state (mapW : int) 
						(mapH : int)
						(screenW : int)
						(screenH : int) =
	object
		val objectWidth = screenW / mapW
		val objectHeight = screenH / mapH
		val gameArray = generateMap mapW mapH screenW screenH
		val player = new player
													(new point (screenW / mapW * 3 / 2)
																		 (screenH / mapH * 3 / 2))
													(screenH / mapH / 2)
													(screenW / mapW)
													(screenH / mapH)

		method movePlayer (dir : char) =
			if not player#moving then
				(let mov = match dir with
				| 'w' -> (0, 1)
				| 'a' -> (-1, 0)
				| 's' -> (0, -1)
				| 'd' -> (1, 0) 
				| _ -> (0, 0) in
				let (oldx, oldy) = player#getArrCoords objectWidth objectHeight in
				let (newx, newy) = oldx + fst mov, oldy + snd mov in 
				if gameArray.(newx).(newy) = Empty then
					player#move (newx * objectWidth + (objectWidth / 2))
											(newy * objectHeight + (objectHeight / 2)))
		
		method drawState =
			Array.iter drawArray gameArray ;
			player#draw
end
