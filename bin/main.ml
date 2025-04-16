let ( let* ) = Lwt.bind

let spotify_proxy bus =
  OBus_proxy.make
    ~peer:
      (OBus_peer.make ~connection:bus ~name:"org.mpris.MediaPlayer2.spotify")
    ~path:[ "org"; "mpris"; "MediaPlayer2" ]

let metadata_init
    proxy =
  let* metadata_monitor =
    OBus_property.monitor
      (Spotify_dbus.Spotify_client.Org_mpris_MediaPlayer2_Player.metadata proxy)
  in
  Lwt.return metadata_monitor

let volume_init proxy : float Lwt_react.signal Lwt.t =
  let* volume_monitor =
    OBus_property.monitor
      (Spotify_dbus.Spotify_client.Org_mpris_MediaPlayer2_Player.volume proxy)
  in
  Lwt.return volume_monitor

let playback_status_init proxy =
  let* pbs_monitor =
    OBus_property.monitor
      (Spotify_dbus.Spotify_client.Org_mpris_MediaPlayer2_Player.playback_status
         proxy)
  in
  Lwt.return pbs_monitor

let rate_init proxy =
  let* rate_monitor =
    OBus_property.monitor
      (Spotify_dbus.Spotify_client.Org_mpris_MediaPlayer2_Player.rate proxy)
  in
  Lwt.return rate_monitor

let position_init proxy =
  let* init_pos =
    OBus_property.get
      (Spotify_dbus.Spotify_client.Org_mpris_MediaPlayer2_Player.position proxy)
  in
  let signal, set = Lwt_react.S.create init_pos in
  Lwt.return (signal, set)

let seeked_init proxy =
  let* seeked_signal =
    OBus_signal.connect
      (Spotify_dbus.Spotify_client.Org_mpris_MediaPlayer2_Player.seeked proxy)
  in
  Lwt.return seeked_signal

(* *** *)

let pr_metadata_full md : unit Lwt.t =
  Lwt_list.iter_p
    (fun (k, v) -> Lwt_io.printf "%s: %s\n" k (OBus_value.V.string_of_single v))
    md

let pr_metadata_artUrl md : unit Lwt.t =
  let v = List.assoc_opt "mpris:artUrl" md in
  let v' = Option.map OBus_value.V.string_of_single v in
  let v'' = Option.value v' ~default:"NOT FOUND" in
  Lwt_io.printf "mpris:artUrl: %s\n" v''

(* NOTE: How to update position_val when next track?
   Check if position_val is greater than length? How to ensure that length always refers to current track? and not next

   I think spotify emits Seeked when track changes, but
   spec says: "[Seeked ]does not need to be emitted when playback starts or when the track changes, unless the track is starting at an unexpected position"

   It seems that spotify Seeked on track change does not start at 0, so not sure if this is "unexpected" or not.
*)

let cli () =
  Lwt_main.run
    (* Setup *)
    (let* bus = OBus_bus.session () in
     let proxy = spotify_proxy bus in

     let* metadata = metadata_init proxy in
     let* volume = volume_init proxy in
     let* pbs = playback_status_init proxy in
     let* position, position_set = position_init proxy in
     let* rate = rate_init proxy in
     let* seeked_signal = seeked_init proxy in

     (* up cursor, erase whole line *)
     let erase = "\o033[A\o033[2K" in

     let state =
       Lwt_react.S.l5 Spotify_dbus.State.S.make metadata volume pbs position
         rate
     in

     (* Attach Actions *)
     let _ = Lwt_react.E.map position_set seeked_signal in
     let _ =
       Lwt_react.S.map
         (fun x -> Lwt_io.printl @@ erase ^ Spotify_dbus.State.S.to_string x)
         state
     in

     let update_position pos setter =
       (* TODO: change 1m to be calculated from rate *)
       let new_pos = Int64.add (Lwt_react.S.value pos) 1_000_000L in
       (* NOTE: This feels hacky, don't know if S.value should be used sparingly or not *)
       let () = setter new_pos in
       Lwt.return_unit
     in
     (* NOTE: recursive loop technique from https://stackoverflow.com/a/40695385/28633986 *)
     let rec update_loop () =
       let* () =
         (* TODO: get from state *)
         Spotify_dbus.State.if_playing
           (Spotify_dbus.State.playback_status_of_string
          @@ Lwt_react.S.value pbs)
           (fun () -> update_position position position_set)
       in

       (* TODO: Handle when Rate = 0 *)
       let sleep_dur = 1. /. Lwt_react.S.value rate in

       (* NOTE: Does not with if `let _` *)
       let* () = Lwt_unix.sleep sleep_dur in
       update_loop ()
     in
     update_loop ())

let gui () =
  Lwt_main.run
    ((* Initializes GTK. *)
     ignore (GMain.init ());

     let* () = Spotify_dbus.Log.out "GTK Initialized" in

     (* Install Lwt<->Glib integration. *)
     Lwt_glib.install ();

     (* Thread which is wakeup when the main window is closed. *)
     let waiter, wakener = Lwt.wait () in

     (* Create a window. *)
     let window = GWindow.window () in

     (* Display something inside the window. *)
     let lab = GMisc.label ~text:"Hello, world!" ~packing:window#add () in

     (* Quit when the window is closed. *)
     ignore (window#connect#destroy (Lwt.wakeup_later wakener));

     (* Show the window. *)
     window#show ();

     let* bus = OBus_bus.session () in
     let proxy = spotify_proxy bus in

     let* metadata = metadata_init proxy in
     let* volume = volume_init proxy in
     let* pbs = playback_status_init proxy in
     let* position, position_set = position_init proxy in
     let* rate = rate_init proxy in
     let* seeked_signal = seeked_init proxy in

     let* () = Spotify_dbus.Log.out "OBus Initialized and connected" in

     let state =
       Lwt_react.S.l5 Spotify_dbus.State.S.make metadata volume pbs position
         rate
     in

     (* Attach Actions *)
     let _ = Lwt_react.E.map position_set seeked_signal in

     let _ =
       Lwt_react.S.map
         (fun s -> lab#set_text @@ Spotify_dbus.State.S.to_string s)
         state
     in

     let update_position pos setter =
       (* TODO: change 1m to be calculated from rate *)
       let new_pos = Int64.add (Lwt_react.S.value pos) 1_000_000L in
       (* NOTE: This feels hacky, don't know if S.value should be used sparingly or not *)
       let () = setter new_pos in
       Lwt.return_unit
     in
     (* NOTE: recursive loop technique from https://stackoverflow.com/a/40695385/28633986 *)
     let rec update_loop () =
       let* () =
         (* TODO: get from state *)
         Spotify_dbus.State.if_playing
           (Spotify_dbus.State.playback_status_of_string
          @@ Lwt_react.S.value pbs)
           (fun () -> update_position position position_set)
       in

       (* TODO: Handle when Rate = 0 *)
       let sleep_dur = 1. /. Lwt_react.S.value rate in

       (* NOTE: Does not with if `let _` *)
       let* () = Lwt_unix.sleep sleep_dur in
       match Lwt.state waiter with
       | Lwt.Return v -> waiter
       (* TODO: Handle Fail (logging) *)
       | Lwt.Fail exn ->
           let* () = Spotify_dbus.Log.err "Waiter Failed" in
           waiter
       | Lwt.Sleep -> update_loop ()
     in
     let* () = update_loop () in
     Spotify_dbus.Log.err "Window closed, exitting gracefully")

(* let () = cli () *)
let () = gui ()

(* TODO: artUrl and displaying images:
  https://i.scdn.co/image/ab67616d0000b27325f8b0dfb1d5619234098cad
  data is a JPEG image data, JFIF standard 1.01, resolution (DPI),
  density 72x72, segment length 16, baseline, precision 8, 640x640, components 3

  Probably `GMisc.image` on a pixbuf? Or file so there can be a cache.
  Then file would be in XDG_CACHE_HOME (https://github.com/ocaml/dune/blob/main/otherlibs/xdg/xdg.mli)

 *)

(* TODO: Figure out how to convert Response to pixbuf/JPEG
         Save Response to File, then use GdkPixbuf.from_file
         (pixbuf has more options to inspect pixels `get_pixels`)
   TODO: Install xdg
*)

(* TODO: For image processing, could look at parallel processing:
   https://ocaml.org/manual/5.0/parallelism.html *)
