open Spotify_client.Org_mpris_MediaPlayer2_Player

let ( let* ) = Lwt.bind

let spotify_proxy bus =
  OBus_proxy.make
    ~peer:
      (OBus_peer.make ~connection:bus ~name:"org.mpris.MediaPlayer2.spotify")
    ~path:[ "org"; "mpris"; "MediaPlayer2" ]

let metadata_init proxy = OBus_property.monitor (metadata proxy)
let volume_init proxy = OBus_property.monitor (volume proxy)
let playback_status_init proxy = OBus_property.monitor (playback_status proxy)
let rate_init proxy = OBus_property.monitor (rate proxy)

let position_init proxy =
  let* init_pos = OBus_property.get (position proxy) in
  let signal, set = Lwt_react.S.create init_pos in
  Lwt.return (signal, set)

let seeked_init proxy = OBus_signal.connect (seeked proxy)
