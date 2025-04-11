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

let if_playing playback_status f =
  match playback_status with
  | Paused | Stopped -> Lwt.return_unit
  | Playing -> f ()

module S : sig
  type t
  type metadata_t = (string * OBus_value.V.single) list
  type volume_t = float
  type position_t = int64
  type rate_t = float

  val make : metadata_t -> volume_t -> string -> position_t -> rate_t -> t
  val to_string : t -> string
end = struct
  type metadata_t = (string * OBus_value.V.single) list
  type volume_t = float
  type position_t = int64
  type rate_t = float

  type t = {
    metadata : metadata_t;
    volume : volume_t;
    playback_status : playback_status_t;
    position : position_t;
    rate : rate_t;
  }

  let make metadata volume playback_status position rate =
    {
      metadata;
      volume;
      playback_status = playback_status_of_string playback_status;
      position;
      rate;
    }

  let artist t =
    let artist = List.assoc_opt "xesam:artist" t.metadata in
    (* TODO: Convert single_list.t to string with delims, no braces, no quotes  *)
    let artist' = Option.map OBus_value.V.string_of_single artist in
    Option.value artist' ~default:"NOT FOUND"

  let title t =
    (* TODO: non-ascii (single quotes, symbols, hangul) are some backslash-escaped numbers *)
    let title = List.assoc_opt "xesam:title" t.metadata in
    let title' = Option.map OBus_value.V.string_of_single title in
    Option.value title' ~default:"NOT FOUND"

  let microsecond_to_minsec (ms : int64) =
    let secs = Int64.div ms 1_000_000L in
    let m = Int64.div secs 60L in
    let s = Int64.rem secs 60L in
    Format.sprintf "%1Li:%02Li" m s

  let to_string t =
    let length = List.assoc_opt "mpris:length" t.metadata in
    (* NOTE: For some reason OBus thinks this is a uint64, but the spec says it is signed *)
    let length' =
      Option.map (OBus_value.C.cast_single OBus_value.C.basic_uint64) length
    in
    let length'' = Option.value length' ~default:0L in
    Format.sprintf "%s - %s, Volume: %5.1f, Status: %7s, %s/%s, (x%3.1f)"
      (artist t) (title t) (100. *. t.volume)
      (playback_status_to_string t.playback_status)
      (microsecond_to_minsec t.position)
      (microsecond_to_minsec length'')
      t.rate
end
