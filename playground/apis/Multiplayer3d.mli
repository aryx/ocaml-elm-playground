(* Multiplayer3d: Multiplayer.mli, for a game in 3D.

   The same modes, chosen by the same flags -- net=local (the default),
   net=simulate, net=host and net=join, net=relay -- and the same
   netcodes: the network carries the players' inputs, never the game,
   so none of it knows or cares that the game draws in 3D. Only the
   view changes: where Multiplayer's [view] gives a player's 2D shapes,
   this one gives a player's camera and scene, and each is drawn in its
   own part of the window (Playground3d.split3d):

   - one screen, the whole window: net=local (player 0's view), and on
     each computer of net=host, net=join or net=relay (that player's);
   - with [~split:true], net=local splits the window, a view per player
     (Playground3d.split);
   - net=simulate: the simulated computers side by side, in columns,
     each its peer's game, labelled with its tick and its netcode's
     numbers (the server's in the middle, for netcode=server);
   - the network's lines (the latency, the checksums, whether the other
     player is there) in a strip along the bottom.

   A player's [view] gets a computer whose screen is its own part of the
   window (Playground3d.area_screen), to lay its HUD out on:

     let view computer n model = (camera_of n model, scene model @ hud computer.screen n model)
     let app network = Multiplayer3d.game3d ~network ~players:2 view update initial_model

   (games/fps/TinyCyberSled.ml, net=simulate, net=host and net=join.) *)

val game3d :
  ?network:< Cap.network ; .. > ->
  ?split:bool ->
  players:int ->
  (Playground.computer -> int -> 'model -> Playground3d.camera * Playground3d.shape3d list) ->
  (Playground.computer -> Multiplayer.player list -> 'model -> 'model) ->
  'model ->
  ('model Multiplayer.state, Playground.msg) Playground3d.app3d
