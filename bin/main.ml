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
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.metadata proxy)
  in
  (* let e = Lwt_react.S.changes metadata_monitor in *)
  Lwt.return metadata_monitor

let volume_init proxy : float Lwt_react.signal Lwt.t =
  let* volume_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.volume proxy)
  in
  (* let e = Lwt_react.S.changes volume_monitor in *)
  Lwt.return volume_monitor

type playback_status_t = Playing | Paused | Stopped

let playback_status_of_string = function
  | "Playing" -> Playing
  | "Paused" -> Paused
  | "Stopped" -> Stopped
  | _ -> failwith "playback_status_of_string: Invalid PlaybackStatus"

let playback_status_to_string = function
  | Playing -> "Playing"
  | Paused -> "Paused"
  | Stopped -> "Stopped"

let playback_status_init proxy =
  let* pbs_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player
       .playback_status proxy)
  in
  Lwt.return (Lwt_react.S.map playback_status_of_string pbs_monitor)

let rate_init proxy =
  let* rate_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.rate proxy)
  in
  Lwt.return rate_monitor

let position_init proxy =
  let* init_pos =
    OBus_property.get
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.position proxy)
  in
  let signal, set = Lwt_react.S.create init_pos in
  Lwt.return (signal, set)

let seeked_init proxy =
  let* seeked_signal =
    OBus_signal.connect
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.seeked proxy)
  in
  Lwt.return seeked_signal

(* * *)

let pr_metadata md =
  let artist = List.assoc_opt "xesam:artist" md in
  (* TODO: Convert single_list.t to string with delims, no braces, no quotes  *)
  let artist' = Option.map OBus_value.V.string_of_single artist in
  let artist'' = Option.value artist' ~default:"NOT FOUND" in
  (* TODO: non-ascii (single quotes, symbols, hangul) are some backslash-escaped numbers *)
  let title = List.assoc_opt "xesam:title" md in
  let title' = Option.map OBus_value.V.string_of_single title in
  let title'' = Option.value title' ~default:"NOT FOUND" in
  Format.sprintf "%s - %s, " artist'' title''

let pr_volume v = Format.sprintf "Volume: %5.1f, " (100.0 *. v)

let pr_playback_status pbs =
  Format.sprintf "Status: %7s" (playback_status_to_string pbs)

let microsecond_to_minsec (ms : int64) =
  let secs = Int64.div ms 1_000_000L in
  let m = Int64.div secs 60L in
  let s = Int64.rem secs 60L in
  Format.sprintf "%1Li:%02Li" m s

let pr_position pos md =
  let length = List.assoc_opt "mpris:length" md in
  (* NOTE: For some reason OBus thinks this is a uint64, but the spec says it is signed *)
  let length' =
    Option.map (OBus_value.C.cast_single OBus_value.C.basic_uint64) length
  in
  let length'' = Option.value length' ~default:0L in
  Format.sprintf " Position: %s/%s"
    (microsecond_to_minsec pos)
    (microsecond_to_minsec length'')

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

let () =
  Lwt_main.run
    (let* bus = OBus_bus.session () in
     let proxy = spotify_proxy bus in

     let* metadata = metadata_init proxy in
     let* volume = volume_init proxy in
     let* pbs = playback_status_init proxy in
     let* position, position_set = position_init proxy in
     let* rate = rate_init proxy in
     let* seeked = seeked_init proxy in

     (* up cursor, erase whole line *)
     let erase_s = Lwt_react.S.const "\o033[A\o033[2K" in

     (* TODO rename these string names *)
     let metadata_s : string React.signal =
       Lwt_react.S.map pr_metadata metadata
     in
     let volume_s : string React.signal = Lwt_react.S.map pr_volume volume in
     let pbs_s = Lwt_react.S.map pr_playback_status pbs in
     let position_s = Lwt_react.S.l2 pr_position position metadata in

     (* TODO: Can I just make a full format function that works (up to l6)? *)
     let state_s =
       Lwt_react.S.merge ( ^ ) ""
         [ erase_s; metadata_s; volume_s; pbs_s; position_s ]
     in

     (* Attached Actions *)
     let _ = Lwt_react.E.map position_set seeked in
     let _ = Lwt_react.S.map Lwt_io.printl state_s in

     let update_position pos setter =
       (* TODO: change 1m to be calculated from rate *)
       let new_pos = Int64.add (Lwt_react.S.value pos) 1_000_000L in
       (* NOTE: This feels hacky, don't know if S.value should be used sparingly or not *)
       let () = setter new_pos in
       Lwt.return_unit
     in
     (* in *)
     let pbs_cond = function
       | Paused | Stopped -> Lwt.return_unit
       | Playing -> update_position position position_set
     in
     (* NOTE: recursive loop technique from https://stackoverflow.com/a/40695385/28633986 *)
     let rec update_loop () =
       let* () = pbs_cond (Lwt_react.S.value pbs) in

       (* TODO: Handle when Rate = 0 *)
       let sleep_dur = 1. /. Lwt_react.S.value rate in

       (* NOTE: Does not with if `let _` *)
       let* () = Lwt_unix.sleep sleep_dur in
       update_loop ()
     in
     update_loop ())
