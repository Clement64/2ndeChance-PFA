open Component_defs

let running = ref true
let init () = ()

let update _dt el =
  List.iter (fun e ->
    (* les composants de e*)
    let pos = Position.get e in
    let box = Box.get e in
    (* les composants du joueur *)
    let posp = Position.get (Game_state.get_player ()) in
    let boxp = Box.get (Game_state.get_player ()) in
    (* la soustraction de Minkowski *)
    let s_pos, s_rect = Rect.mdiff posp boxp pos box in
    if Rect.has_origin s_pos s_rect then 
      begin
        let ctx = Draw_system.get_canvas () in
        Gfx.clear_rect ctx 0 0 800 600;
        Gfx.draw_text ctx "Win" (300) (400) "Arial 30px" (Gfx.color 0 255 0 255);
        running := false;
      end
  ) el
