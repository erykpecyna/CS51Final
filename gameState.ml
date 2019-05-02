open Array ;;
open Gameobj ;;
open Util ;;

module type GAMESTATE =
sig
	type state
	val state
end

let module BomberState : GAMESTATE =
struct
	(* Generates a new map with boxes placed to allow for a proper start for all
		players. Takes arguments for the size of the map. *)
	let generateMap (w : int)
									(h : int)
									(* ?(screenwidth : int = 800)
									?(screenheight : int = 600) =  *) =
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
                                    		| Wall w -> w#draw
                                    		| Box b -> b#draw)
                      			arr ;;



	class state (mapW : int) 
							(mapH : int)
							(screenW : int)
							(screenH : int) =
		object
			val mutable mapWidth = 15
			val mutable mapHeight = 13
			val mutable objectWidth = screenWidth / mapWidth
			val mutable objectHeight = screenHeight / mapHeight
			val mutable gameArray = generateMap mapW mapH

			method drawMap = Array.iter drawArray gameArray
			
		end

end