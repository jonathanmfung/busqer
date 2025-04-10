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

let playback_status_init proxy =
  let* pbs_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player
       .playback_status proxy)
  in
  Lwt.return pbs_monitor

let rate_init proxy =
  let* rate_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.rate proxy)
  in
  Lwt.return rate_monitor
(* TODO: Need a way to update position signal (NOT OBus signal)
   This needs to tick every 1 second (x rate):
   Lwt_react.S.map +1e6 position_s
   which I guess top level `run` in main does, but feels too decoupled (rate update vs ui update)
   (don't see how `Lwt_unix.sleep 1.0` would be modified at runtime, if rate changes;
   arg could be rate instead of a literal)
*)

let position_init proxy =
  let* position_monitor =
    OBus_property.monitor
      (Spotify_dbus__Spotify_client.Org_mpris_MediaPlayer2_Player.position proxy)
  in
  Lwt.return position_monitor

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
let pr_playback_status pbs = Format.sprintf "Status: %7s" pbs

let microsecond_to_minsec (ms : int64) =
  let secs = Int64.div ms 1_000_000L in
  let m = Int64.div secs 60L in
  let s = Int64.rem secs 60L in
  Format.sprintf "%1Li:%02Li" m s

(* TODO: pr_position requires both position and metadata.length *)
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

(* TODO: position is taken from Position property
   Position is not monitorable, need to use Rate property to manually step position
   track length is from Metadata mpris:length
   listen to Seeked signal to also update position
     (This also needs PlaybackStatus property)
*)

let () =
  Lwt_main.run
    (let rec run () =
       (* NOTE: recursive loop technique from https://stackoverflow.com/a/40695385/28633986 *)
       (* TODO: this sleep doesn't actually block output *)
       let* () = Lwt_unix.sleep 1.0 in
       run ()
     in
     let* bus = OBus_bus.session () in
     let proxy = spotify_proxy bus in
     let* metadata = metadata_init proxy in
     let* volume = volume_init proxy in
     let* pbs = playback_status_init proxy in
     let* position = position_init proxy in
     (* let erase_s = Lwt_react.S.const "\o033[A\o033[2K" in *)
     (* up cursor, erase whole line *)
     let metadata_s : string React.signal =
       Lwt_react.S.map pr_metadata metadata
     in
     let volume_s : string React.signal = Lwt_react.S.map pr_volume volume in
     let pbs_s = Lwt_react.S.map pr_playback_status pbs in
     let position_s = Lwt_react.S.l2 pr_position position metadata in
     let state_s =
       Lwt_react.S.merge ( ^ ) ""
         [ (* erase_s;  *) metadata_s; volume_s; pbs_s; position_s ]
     in
     let _ = Lwt_react.S.map Lwt_io.printl state_s in
     run ())
