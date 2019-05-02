open Array ;;
open Gameobj ;;
open Util ;;

(* Generates a new map with boxes placed to allow for a proper start for all
	players. Takes arguments for the size of the map. *)
let generateMap (w : int)
								(h : int)
								(screenwidth : int)
								(screenheight : int) =  =
	(* + 2 accounts for border around playable area *)
	let xwidth = screenWidth / (w + 2) in
	let ywidth = screenHeight / (h + 2) in
	let map = make_matrix (w + 2) (h + 2) Empty in
	for x = 0 to (w + 1) do
		for y = 0 to (h + 1) do
		if (x = 0 || x = w + 1 || y = 0 || y = h + 1
				|| (x mod 2 = 0 && y mod 2 = 0)) then
			map.(x).(y) <- Wall (new wall 
																(new point (x*xwidth) (y*ywidth))
																xwidth
																ywidth)
		else
		let rand = Random.int 2 in
		if (rand = 1) then map.(x).(y) <- Box (new box 
																						(new point (x*xwidth) (y*ywidth))
																						xwidth
																						ywidth)
		else map.(x).(y) <- Empty
		done
	done ;
	map.(1).(1) <- Empty ;
	map.(2).(1) <- Empty ;
	map.(1).(2) <- Empty ;
	map.(1).(h) <- Empty ;
	map.(1).(h-1) <- Empty ;
	map.(2).(h) <- Empty ;
	map.(w).(1) <- Empty ;
	map.(w).(2) <- Empty ;
	map.(w-1).(1) <- Empty ;
	map.(w-1).(h) <- Empty ;
	map.(w).(h) <- Empty ;
	map.(w).(h-1) <- Empty ;
	map ;;

let drawArray = 
	fun arr -> Array.iter (fun obj -> match obj with
																		| Empty -> ()
																		| _ b -> b#draw )
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
													(new point (objectWidth * 3 / 2)
													(objectHeight * 3 / 2))
													(objectHeight / 2)

		method movePlayer (dir : char) =
			let mov = match dir with
			| 'w' -> (0, 1)
			| 'a' -> (-1, 0)
			| 's' -> (0, -1)
			| 'd' -> (1, 0) in
			let (oldx, oldy) = player#getArrCoords screenW screenH in
			let (newx, newy) = oldx + fst mov, oldy + snd mov in 
			if gameArray.(newx).(newy) = Empty then
				player#move (newx * objectWidth) (newy * objectHeight)
		
		method drawState =
			Array.iter drawArray gameArray ;
			player#draw

	end
