open Component_defs

let ctx = ref None
let init () =
  let _, c = Gfx.create 
  "game_canvas:800x600:" 
  in
  ctx := Some c

let get_canvas () = Option.get !ctx

(*Dimensions hardcodées pour scrolling horizontal*)
let wplayer = 32
let wscreen = 800
let wlevel = 8000

let update _dt el =
  let ctx = Option.get !ctx in
  Gfx.clear_rect ctx 0 0 800 600;
  List.iter (fun e ->
    let playerpos = Position.get (Game_state.get_player ()) in
    let pos = Position.get e in
    let screenxpos = Stdlib.max 0 (Stdlib.min (wlevel - wscreen) ((int_of_float playerpos.x + wplayer / 2) - (wscreen / 2))) in
    let box = Box.get e in
    let surface = Surface.get e in (* Question 3.2 *)
    match surface with
    Color color -> Gfx.fill_rect ctx (int_of_float pos.x - screenxpos)
                         (int_of_float pos.y)
                          box.width
                          box.height
                          color

                          (* Question 3.3 *)
    | Image render -> Gfx.blit_scale ctx render (int_of_float pos.x - screenxpos)
                                                (int_of_float pos.y)
                                                box.width
                                                box.height
    | Animation anim -> (* Question 4.5 *)
                let v = Velocity.get e in
                let d = if v.x < 0.0 then -1 else if v.x > 0.0 then 1 else 0 in
                let render = Texture.get_frame anim d in
                Gfx.blit_scale ctx render (int_of_float pos.x - screenxpos)
                (int_of_float pos.y) box.width box.height
    | Text (text, font, color) ->
           Gfx.draw_text ctx text (int_of_float pos.x - screenxpos) (int_of_float pos.y) font color
    ) el;
    Gfx.commit ctx
