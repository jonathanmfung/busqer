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

let metadata_init proxy :
    ((string * OBus_value.V.single) list React.event * 'a) Lwt.t =
  let* metadata_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.metadata proxy)
  in
  let e = Lwt_react.S.changes metadata_monitor in
  let rec r () =
    let* () = Lwt_unix.sleep 1.0 in
    r ()
  in
  Lwt.return (e, r)

let pr_metadata md : unit Lwt.t =
  Lwt_list.iter_p
    (fun (k, v) -> Lwt_io.printf "%s: %s\n" k (OBus_value.V.string_of_single v))
    md

let () =
  Lwt_main.run
    (let* bus = OBus_bus.session () in
     let proxy = spotify_proxy bus in
     let* metadata, run = metadata_init proxy in
     let _ = Lwt_react.E.map pr_metadata metadata in
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
