(* NOTE: How to update position_val when next track?
   Check if position_val is greater than length? How to ensure that length always refers to current track? and not next

   I think spotify emits Seeked when track changes, but
   spec says: "[Seeked ]does not need to be emitted when playback starts or when the track changes, unless the track is starting at an unexpected position"

   It seems that spotify Seeked on track change does not start at 0, so not sure if this is "unexpected" or not.
*)

let ( let* ) = Lwt.bind

open Busqer

let gui () =
  Lwt_main.run
    ((* * GTK Init * *)
     ignore (GMain.init ());

     let* () = Log.out "GTK Initialized" in

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
     track_info_w#set_name "track_info";
     let length_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     length_w#set_name "length";
     let art_url_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     art_url_w#set_name "art_url";
     let volume_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     volume_w#set_name "volume";
     let playback_status_w =
       GMisc.label ~text:"Hello, world!" ~packing:vbox#pack ()
     in
     playback_status_w#set_name "playback_status";
     let position_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     position_w#set_name "position";
     let rate_w = GMisc.label ~text:"Hello, world!" ~packing:vbox#pack () in
     rate_w#set_name "rate";

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
     let rng_elt arr =
       let n = Random.int (Array.length arr) in
       Array.get arr n
     in

     let mat_to_centroids (m : Lacaml.S.mat) =
       let tree = OtsuPcaPart.cluster m 3 in
       Lwt.map OtsuPcaPart.centroids tree
     in

     let centroids_to_ran_color c =
       OtsuPcaPart.to_hexstring (rng_elt (Array.of_list c))
     in

     let css_provider_from_data data =
       let provider = GObj.css_provider () in
       provider#load_from_data data;
       provider
     in

     let mk_provider (css : string) =
       (css_provider_from_data css)#as_css_provider
     in
     let add_provider_to_screen css_prov =
       GtkData.StyleContext.add_provider_for_screen (Gdk.Screen.default ())
         css_prov GtkData.StyleContext.ProviderPriority.application
     in
     let remove_provider_from_screen css_prov =
       GtkData.StyleContext.remove_provider_for_screen (Gdk.Screen.default ())
         css_prov
     in
     (* TODO: Making full css string will be horrible because it means all FRP components feed into one big thing,
        which will probably break the concurrency law
     *)
     let css_str color =
       Format.sprintf
         "* { color: blue } label#track_info {background-color: %s}" color
     in

     (* track_info_w#misc#style_context#add_provider (\* Does not cascade! *\) *)
     (*   (css_provider_from_data "* { background-color: red }") *)
     (*   GtkData.StyleContext.ProviderPriority.application; *)
     (* let* () = Lwt_unix.sleep 5.0 in *)
     (* track_info_w#misc#style_context#add_provider (\* Does not cascade! *\) *)
     (*   (css_provider_from_data "* { background-color: purple }") *)
     (*   GtkData.StyleContext.ProviderPriority.application; *)

     let pb_init = GdkPixbuf.create ~width:1 ~height:1 () in
     let img = GMisc.image ~pixbuf:pb_init ~packing:vbox#add () in

     (* TODO:
        Okay:
        GdkPixbuf.get_pixels (GdkPixbuf.from_file "./test/data/black_10x10.jpg");;
        seg fault:
        GdkPixbuf.get_pixels (GdkPixbuf.from_file "./test/data/ab67616d0000b27338d7a50443e2a6043d6da247.jpg");;

        get_pixels seems to have some issues: https://github.com/bcpierce00/unison/issues/1075
     *)
     let colors = [| "#ff0000"; "#00ff00"; "#0000ff" |] in

     let _update_album_art =
       Lwt_react.S.map
         (fun au ->
           Lwt_result.bind au (fun a ->
               let open Lwt.Infix in
               let cache_res = (let* a = Lwt_preemptive.detach ArtUrl.A.to_cache a in Lwt.return ()) in

               let a_filepath = ArtUrl.A.abs_path a in
               let pb = Lwt_preemptive.detach GdkPixbuf.from_file a_filepath in
               let setter = pb >>= (fun x -> let* a = Lwt_preemptive.detach img#set_pixbuf x in Lwt.return ())  in

               (* let mat = Lwt_preemptive.detach ArtUrl.stb_to_mat a_filepath in *)
               (* let mat' = mat >>= fun x -> match x with *)
               (*   | Result.Error e -> failwith "stb fail load" *)
               (*   | Result.Ok x -> Lwt.return x in *)

               let mat' = ArtUrl.stb_to_mat a_filepath in

               (* let mat' = ArtUrl.jpg_to_mat a_filepath in *)

               (* let mat_res = mat' >>= fun x -> let () = track_info_w#set_text (rng_elt colors) in *)
               (*                                 Log.err "mat dim2: %i" (Lacaml.S.Mat.dim2 x) in *)
               let mat_res = mat'
                             |> Lwt_result.map_error (fun _ -> Failure "mat failure")
                             |> Lwt_result.get_exn >>=
                               fun m -> let () = track_info_w#set_text (rng_elt colors) in
                                        Log.err "mat dim2: %i" (Lacaml.S.Mat.dim2 m) in

               (* let mat_res = (let* a = Lwt_preemptive.detach ArtUrl.stb_to_mat a_filepath in Lwt.return () ) in *)

               (* Lwt_result.return @@ Lwt.join [cache_res; setter] *)
               (* Lwt_result.return @@ Lwt.join [setter; mat_res] *)
               Lwt_result.return @@ Lwt.join [cache_res; setter; mat_res]
              (* Lwt_result.return mat_res *)
             ))
         art_url
     in

     (* let _update_album_color = *)
     (*   Lwt_react.S.map *)
     (*     (fun au -> *)
     (*       Lwt_result.bind au (fun a -> *)
     (*           (\* TODO: Need to understand how (Lwt)_React handles queues of updates (e.g. skipping track many times) *\) *)
     (*           let a_filepath = ArtUrl.A.abs_path a in *)
     (*           (\* let a_filepath = "/home/jonat/ocaml/busqer/test/data/black_10x10.jpg" in *\) *)
     (*           let ( let* ) = Lwt.bind in *)
     (*           (\* TODO: jpg_to_mat AND stb_to_mat block GUI *\) *)
     (*           (\* let* mat = ArtUrl.jpg_to_mat a_filepath in *\) *)
     (*           (\* let mat = ArtUrl.stb_to_mat a_filepath in *\) *)
     (*           let* mat = Lwt.catch *)
     (*                        (fun () -> Lwt_preemptive.detach ArtUrl.stb_to_mat a_filepath) *)
     (*                        (function *)
     (*                         | exn -> *)
     (*                            let*()=Log.err "stb_to_mat fail" in *)
     (*                            failwith(Format.sprintf "stb_to_mat failed with: %s" (Printexc.to_string exn))) in *)
     (*           let* mat' = match mat with *)
     (*             | Result.Error e -> failwith "stb fail load" *)
     (*             | Result.Ok x -> Lwt.return x in *)
     (*           let* () = Log.err "mat dim2: %i" (Lacaml.S.Mat.dim2 mat') in *)
     (*           let* centroids = mat_to_centroids mat' in *)
     (*           let color = centroids_to_ran_color centroids in *)
     (*           let prov = mk_provider (css_str color) in *)
     (*           let* () = Log.err "ran_color: %s" color in *)
     (*           Lwt_result.return *)
     (*           @@ Lwt_mutex.with_lock mx_gui (fun () -> *)
     (*                  track_info_w#set_text color; *)
     (*                  Lwt_preemptive.detach add_provider_to_screen prov *)
     (*                (\* Log.out "test from inside mutex" *\) *)
     (*                (\* Lwt.return () *\) *)
     (*                ) *)
     (*     )) *)
     (*     art_url *)
     (* in *)

     (* TODO: GUI freezes when program is started and current track's art_url needs to be url fetched *)

     let* () = Lwt_unix.sleep 0.1 in (* Give GUI time to setup? *)

     (* Update Position *)
     let update_position_tick pos setter =
       (* TODO: change 1m to be calculated from rate *)
       let new_pos = Int64.add (Lwt_react.S.value pos) 1_000_000L in
       (* NOTE: This feels hacky, don't know if S.value should be used sparingly or not *)
       let () = setter new_pos in
       Lwt.return ()
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
       (* let* () = Log.err "Sleeping for %1.2f" sleep_dur in *)
       let* () = Lwt_unix.sleep sleep_dur in
       match Lwt.state waiter with
       | Lwt.Return v -> waiter
       | Lwt.Fail exn ->
           let* () = Log.err "Waiter Failed: %s" (Printexc.to_string exn) in
           waiter
       | Lwt.Sleep -> update_loop ()
     in
     let* () = update_loop () in
     Log.err "Window closed, exitting gracefully")

let () =
  Printexc.record_backtrace true;
  try gui ()
  with e ->
    let name, msg = OBus_error.cast e in
    Printf.printf "DBus error (%s): %s" name msg

(* TODO: Install xdg *)

(* TODO: For image processing, could look at parallel processing:
   https://ocaml.org/manual/5.0/parallelism.html *)

type _ Effect.t += Xchg : int -> int Effect.t
type _ Effect.t += WriteFile : string -> unit Effect.t

let write_file f = Effect.perform (WriteFile f)
let comp1 () : int = Effect.perform (Xchg 0) + Effect.perform (Xchg 1)
