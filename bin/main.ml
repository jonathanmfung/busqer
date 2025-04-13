let ( let* ) = Lwt.bind

let spotify_proxy bus =
  OBus_proxy.make
    ~peer:
      (OBus_peer.make ~connection:bus ~name:"org.mpris.MediaPlayer2.spotify")
    ~path:[ "org"; "mpris"; "MediaPlayer2" ]

let metadata_init
    proxy (* : (string * OBus_value.V.single) list React.event Lwt.t *) =
  let* metadata_monitor =
    OBus_property.monitor
      (Spotify_dbus.Spotify_client.Org_mpris_MediaPlayer2_Player.metadata proxy)
  in
  (* let e = Lwt_react.S.changes metadata_monitor in *)
  Lwt.return metadata_monitor

let volume_init proxy : float Lwt_react.signal Lwt.t =
  let* volume_monitor =
    OBus_property.monitor
      (Spotify_dbus.Spotify_client.Org_mpris_MediaPlayer2_Player.volume proxy)
  in
  (* let e = Lwt_react.S.changes volume_monitor in *)
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
     let erase_s = Lwt_react.S.const "\o033[A\o033[2K" in

     let state =
       Lwt_react.S.l5 Spotify_dbus.State.S.make metadata volume pbs position
         rate
     in

     (* Attach Actions *)
     let _ = Lwt_react.E.map position_set seeked_signal in
     let _ =
       Lwt_react.S.map
         (fun x -> Lwt_io.printl @@ Spotify_dbus.State.S.to_string x)
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

open Bogue
module W = Widget
module L = Layout
module T = Trigger

let gui () =
  (* Bogue Example 15 *)
  (* This works by attatching an infinite loop to the widget.
     The infinite loop is activated on the startup trigger event
     The connections are such that the source and target are the same, but the `clock` action only cares about the source. The target is just a necessary dummy paramter.
  *)
  let clock_with_prefix prefix w_source _ ev =
    let signal, signal_set = Lwt_react.S.create 0 in
    let prev = ref @@ Unix.gettimeofday () in
    let _ = Lwt_react.S.map (Format.printf "%i") signal in

    let set_w_source s =
      Label.set (W.get_label w_source) (prefix ^ Format.sprintf "%i" s)
    in
    let _ = Lwt_react.S.map set_w_source signal in

    let rec loop () =
      let now = Unix.gettimeofday () in
      if now -. !prev > 1.0 then (
        prev := now;
        signal_set (succ @@ Lwt_react.S.value signal));
      W.update w_source;
      Thread.delay 0.25;
      if T.should_exit ev then (
        print_endline "Stopping Clock";
        T.will_exit ev)
      else loop ()
    in
    print_endline "Starting new clock";
    loop ()
  in
  let clock = clock_with_prefix "Test: " in
  let l = W.label ~size:40 "Autostarts" in
  let c = W.connect l l clock [ T.startup ] in
  let lay = L.flat_of_w [ l ] in
  let board = Bogue.make [ c ] [ lay ] in
  Bogue.run board;
  Draw.quit ()


(* let () = cli () *)
let () = gui ()
