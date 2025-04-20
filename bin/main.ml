(* NOTE: How to update position_val when next track?
   Check if position_val is greater than length? How to ensure that length always refers to current track? and not next

   I think spotify emits Seeked when track changes, but
   spec says: "[Seeked ]does not need to be emitted when playback starts or when the track changes, unless the track is starting at an unexpected position"

   It seems that spotify Seeked on track change does not start at 0, so not sure if this is "unexpected" or not.
*)

let ( let* ) = Lwt.bind
open Spotify_dbus

let gui () =
  Lwt_main.run
    ((* * GTK Init * *)
     ignore (GMain.init ());

     let* () = Spotify_dbus.Log.out "GTK Initialized" in

     (* Install Lwt<->Glib integration. *)
     Lwt_glib.install ();

     (* Thread which is wakeup when the main window is closed. *)
     let waiter, wakener = Lwt.wait () in

     (* Create a window. *)
     let window = GWindow.window () in

     (* Display something inside the window. *)
     (* https://github.com/garrigue/lablgtk/blob/lablgtk3/examples/stackcontainer.ml *)
     let vbox = GPack.vbox ~packing:window#add () in

     let lab = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in

     (* Widgets *)
     let track_info_w =
       GMisc.label ~text:"Hello, world!" ~packing:vbox#pack ()
     in
     let length_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     let art_url_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     let volume_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     let playback_status_w =
       GMisc.label ~text:"Hello, world!" ~packing:vbox#pack ()
     in
     let position_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     let rate_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in

     (* Quit when the window is closed. *)
     ignore (window#connect#destroy (Lwt.wakeup_later wakener));

     (* Show the window. *)
     window#show ();

     (* * DBus Setup * *)
     let* bus = OBus_bus.session () in
     let proxy = Dbus.spotify_proxy bus in

     (* Initial DBus monitors *)
     let* metadata_ = Dbus.metadata_init proxy in
     let* volume_ = Dbus.volume_init proxy in
     let* playback_status_ = Dbus.playback_status_init proxy in
     let* position_, position_set = Dbus.position_init proxy in
     let* rate_ = Dbus.rate_init proxy in
     let* seeked_signal = Dbus.seeked_init proxy in

     (* Derived DBus monitor-specific types *)
     let track_info =
       Lwt_react.S.map State.Metadata.make_track_info metadata_
     in
     let length = Lwt_react.S.map State.Metadata.make_length metadata_ in
     let art_url = Lwt_react.S.map State.Metadata.make_art_url metadata_ in
     let volume = Lwt_react.S.map State.Volume.make volume_ in
     let playback_status =
       Lwt_react.S.map State.PlaybackStatus.make playback_status_
     in
     let position = Lwt_react.S.map State.Position.make position_ in
     let rate = Lwt_react.S.map State.Rate.make rate_ in

     let* () = Log.out "OBus Initialized and connected" in

     (* * Attach Actions * *)
     (* Seek Position *)
     let _position_seek = Lwt_react.E.map position_set seeked_signal in

     (* * Update UI * *)
     let mx_gui = Lwt_mutex.create () in
     (* TODO: Get rid of mutex because GTK should be able to handle concurrent updates
        (??? not sure how lablgtk binding handles this) *)
     let _update_track_info =
       Lwt_react.S.map
         (fun s ->
           Lwt_mutex.with_lock mx_gui (fun () ->
               Lwt.return @@ track_info_w#set_text @@ State.Metadata.to_string s))
         track_info
     in

     let _update_length =
       Lwt_react.S.map
         (fun s ->
           Lwt_mutex.with_lock mx_gui (fun () ->
               Lwt.return @@ length_w#set_text @@ State.Metadata.to_string s))
         length
     in

     let _update_volume =
       Lwt_react.S.map
         (fun s ->
           Lwt_mutex.with_lock mx_gui (fun () ->
               Lwt.return @@ volume_w#set_text @@ State.Volume.to_string s))
         volume
     in

     let _update_playback_status =
       Lwt_react.S.map
         (fun s ->
           Lwt_mutex.with_lock mx_gui (fun () ->
               Lwt.return @@ playback_status_w#set_text
               @@ State.PlaybackStatus.to_string s))
         playback_status
     in
     let _update_position =
       Lwt_react.S.map
         (fun s ->
           Lwt_mutex.with_lock mx_gui (fun () ->
               Lwt.return @@ position_w#set_text @@ State.Position.to_string s))
         position
     in

     let _update_rate =
       Lwt_react.S.map
         (fun s ->
           Lwt_mutex.with_lock mx_gui (fun () ->
               Lwt.return @@ rate_w#set_text @@ State.Rate.to_string s))
         rate
     in

     (* Update Album Art *)
     let pb_init = GdkPixbuf.create ~width:1 ~height:1 () in
     let img = GMisc.image ~pixbuf:pb_init ~packing:vbox#add () in

     let _update_album_art =
       Lwt_react.S.map
         (fun au ->
           Lwt_result.bind au
             Spotify_dbus.(
               fun a ->
                 let ( let* ) = Lwt_result.bind in
                 let* () =
                   Lwt_result.map_error (fun e -> [ e ]) (ArtUrl.A.to_cache a)
                 in
                 let pb = GdkPixbuf.from_file @@ ArtUrl.A.abs_path a in
                 Lwt_result.return
                 @@ Lwt_mutex.with_lock mx_gui (fun () ->
                        Lwt.return @@ img#set_pixbuf pb)))
         art_url
     in

     (* TODO: GUI freezes when program is started and current track's art_url needs to be url fetched *)

     (* Update Position *)
     let update_position_tick pos setter =
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
         State.PlaybackStatus.if_playing (Lwt_react.S.value playback_status)
           (fun () ->
             Lwt_mutex.with_lock mx_gui @@ fun () ->
             update_position_tick position position_set)
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

let () =
  Printexc.record_backtrace true;
  try gui ()
  with e ->
    let name, msg = OBus_error.cast e in
    Printf.printf "DBus error (%s): %s" name msg

(* TODO: pixbuf has more options to inspect pixels `get_pixels`
   TODO: Install xdg
*)

(* TODO: For image processing, could look at parallel processing:
   https://ocaml.org/manual/5.0/parallelism.html *)
