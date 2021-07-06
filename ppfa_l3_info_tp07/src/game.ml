open Ecs

let chain_functions f_list =
  let funs = ref f_list in
  fun dt -> match !funs with
               [] -> false
              | f :: ll ->
                 if f dt then true
                 else begin
                  funs := ll;
                  true
                 end


let load_level _dt = 
	let ic = open_in "/static/files/level1.txt" in
	let () =
	try
		let count = ref 0 in
		while true do
			let line = input_line ic in
			match String.split_on_char ',' line with
			| [ id; sx; sy; sw; sh ] -> if id = "g" then begin
											ignore (Winwall.create ("wall" ^ string_of_int !count)
											(float_of_string sx)
											(float_of_string sy)
											(int_of_string sw)
											(int_of_string sh));
											count := !count + 1;
											end
										else if id = "r" then begin
											ignore (Losewall.create ("wall" ^ string_of_int !count)
											(float_of_string sx)
											(float_of_string sy)
											(int_of_string sw)
											(int_of_string sh));
											count := !count + 1;
											end
										else begin
											ignore (Wall.create ("wall" ^ string_of_int !count)
											(float_of_string sx)
											(float_of_string sy)
											(int_of_string sw)
											(int_of_string sh));
											count := !count + 1; 
											end
													
			| _ ->  ()
		done
	with End_of_file -> ()
	in
	false

let init_player _dt =
	let player = Player.create "player" 100.0 300.0 in
	Game_state.init player;
	Input_handler.register_command (KeyDown "w") (Player.jump);
	Input_handler.register_command (KeyUp "w") (Player.stop_jump);
	
  Input_handler.register_command (KeyDown "a") (Player.run_left);
  Input_handler.register_command (KeyUp "a") (Player.stop_run_left);
  Input_handler.register_command (KeyDown "d") (Player.run_right);
  Input_handler.register_command (KeyUp "d") (Player.stop_run_right);
	System.init_all ();
	false

let play_game dt =
	Player.do_move ();
	System.update_all dt;
	(!Win_system.running && !Loss_system.running)


let run () =
	Gfx.main_loop
	(chain_functions [
		load_level;
		init_player;
		play_game
	]);
	