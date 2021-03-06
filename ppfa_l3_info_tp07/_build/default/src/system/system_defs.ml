open Ecs

module Control_S = System.Make (Control_system)
module Draw_S = System.Make(Draw_system)
module Move_S = System.Make(Move_system)
module Collision_S = System.Make(Collision_system)
module Force_S = System.Make(Force_system)
module Win_S = System.Make(Win_system)
module Loss_S = System.Make(Loss_system)


let () =
  System.register (module Draw_S);
  System.register (module Move_S);
  System.register (module Collision_S);
  System.register (module Force_S);
  System.register (module Control_S);
  System.register (module Win_S);
  System.register (module Loss_S)
