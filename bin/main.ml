let ( let* ) = Lwt.bind

(* let _first = Lwt_main.run begin *)
(*   (\* Connect to the session bus *\) *)
(*   let* bus = OBus_bus.session () in *)

(*   (\* Create a proxy for a remote object *\) *)
(*   let proxy = *)
(*     OBus_proxy.make *)
(*       ~peer:(OBus_peer.make ~connection:bus ~name:"org.mpris.MediaPlayer2.spotify") *)
(*       ~path:["org"; "mpris"; "MediaPlayer2"] in *)

(*   (\* Call a method *\) *)
(*   let* _result = Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.play_pause proxy in *)

(*   (\* Read the contents of a property *\) *)
(*   let* pos = OBus_property.get (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.position proxy) in *)
(*   let* volume = OBus_property.get (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.volume proxy) in *)
(*   let* metadata = OBus_property.get (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.metadata proxy) in *)

(*   (\* let* metadata_monitor = OBus_property.monitor (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.metadata proxy) in *\) *)

(*   (\* let* () = *\) *)
(*   (\*   Lwt_react.E.notify (fun args -> ...) *\) *)
(*   (\*   =|< OBus_signal.connect (Spotify_dbus__Spotify_client.Org_foo_bar.plip proxy) *\) *)

(*   let _ = Lwt_io.printf "%Ld\n" pos in *)
(*   let _ = Lwt_io.printf "%f\n" volume in *)
(*   Lwt_list.iter_p (fun (k, v) -> Lwt_io.printf "%s: %s\n" k (OBus_value.V.string_of_single v)) metadata; *)
(* end *)

(* ************************************************************************** *)
(* TODO: Lwt_react.S.changes to convert monitor to event
         main is an infinite loop that prints the metadata
*)

let spotify_proxy bus =
  OBus_proxy.make
    ~peer:
      (OBus_peer.make ~connection:bus ~name:"org.mpris.MediaPlayer2.spotify")
    ~path:[ "org"; "mpris"; "MediaPlayer2" ]

let metadata_init
    proxy (* : (string * OBus_value.V.single) list React.event Lwt.t *) =
  let* metadata_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.metadata proxy)
  in
  (* let e = Lwt_react.S.changes metadata_monitor in *)
  Lwt.return metadata_monitor

let volume_init proxy (* : float React.event Lwt.t *) =
  let* volume_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.volume proxy)
  in
  (* let e = Lwt_react.S.changes volume_monitor in *)
  Lwt.return volume_monitor

let pr_metadata_full md : unit Lwt.t =
  Lwt_list.iter_p
    (fun (k, v) -> Lwt_io.printf "%s: %s\n" k (OBus_value.V.string_of_single v))
    md

let pr_metadata_artUrl md : unit Lwt.t =
  let v = List.assoc_opt "mpris:artUrl" md in
  let v' = Option.map OBus_value.V.string_of_single v in
  let v'' = Option.value v' ~default:"NOT FOUND" in
  Lwt_io.printf "mpris:artUrl: %s\n" v''

let pr_volume v = Format.sprintf "Volume: %5.1f" (100.0 *. v)

let pr_metadata md =
  let artist = List.assoc_opt "xesam:artist" md in
  (* TODO: Convert single_list.t to string with delims, no braces  *)
  let artist' = Option.map OBus_value.V.string_of_single artist in
  let artist'' = Option.value artist' ~default:"NOT FOUND" in
  let title = List.assoc_opt "xesam:title" md in
  let title' = Option.map OBus_value.V.string_of_single title in
  let title'' = Option.value title' ~default:"NOT FOUND" in
  Format.sprintf "%s - %s, " artist'' title''

let () =
  Lwt_main.run
    (let rec run () =
       (* TODO: this sleep doesn't actually block output *)
       let* () = Lwt_unix.sleep 1.0 in
       run ()
     in
     let* bus = OBus_bus.session () in
     let proxy = spotify_proxy bus in
     let* metadata = metadata_init proxy in
     let* volume = volume_init proxy in
     let metadata_s : string React.signal =
       Lwt_react.S.map pr_metadata metadata
     in
     let volume_s : string React.signal = Lwt_react.S.map pr_volume volume in
     let state_s = Lwt_react.S.merge ( ^ ) "" [ metadata_s; volume_s ] in
     let _ = Lwt_react.S.map Lwt_io.printl state_s in
     run ())

(* let () = *)
(*   let i = ref 0 in *)
(*   let rec loop () = *)
(*     let* () = Lwt_io.printf "%i\n" !i in *)
(*     i := !i + 1; *)
(*     let* () = Lwt_unix.sleep 1.0 in *)
(*     loop () *)
(*   in *)
(*   Lwt_main.run (loop ()) *)

(* https://stackoverflow.com/a/40695385/28633986 *)
(* let () = *)
(*   let rec echo_loop () = *)
(*     let* line = Lwt_io.(read_line stdin) in *)
(*     if line = "exit" then Lwt.return_unit *)
(*     else *)
(*       let* () = Lwt_io.(write_line stdout line) in *)
(*       echo_loop () *)
(*   in *)
(*   Lwt_main.run (echo_loop ()) *)

(* ************************************************************************** *)
(* let pr_time (t : float) : unit = *)
(*   let tm = Unix.localtime t in *)
(*   Printf.printf "\x1B[8D%02d:%02d:%02d%!" tm.Unix.tm_hour tm.Unix.tm_min *)
(*     tm.Unix.tm_sec *)

(* let clock_init () = *)
(*   let e, send = React.E.create () in *)
(*   let r () = *)
(*     while true do *)
(*       send (Unix.gettimeofday ()); *)
(*       Unix.sleep 1 *)
(*     done *)
(*   in *)
(*   (e, r) *)

(* let _clock_main = *)
(*   let (seconds : float React.event), (run : unit -> 'a) = clock_init () in *)
(*   let _ = React.E.map pr_time seconds in *)
(*   run () *)
