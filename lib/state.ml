let microsecond_to_minsec (ms : int64) =
  let secs = Int64.div ms 1_000_000L in
  let m = Int64.div secs 60L in
  let s = Int64.rem secs 60L in
  Format.sprintf "%1Li:%02Li" m s

type track_info = { title : string; artist : string }

module Metadata : sig
  type t = [ `TrackInfo of track_info | `Length of int64 ]
  type metadata_t = (string * OBus_value.V.single) list

  (* TODO: Consider splitting up TrackInfo into Title, Artist, Album *)
  val make_track_info : metadata_t -> [> `TrackInfo of track_info ]
  val make_length : metadata_t -> [> `Length of int64 ]
  val make_art_url : metadata_t -> (ArtUrl.A.t, ArtUrl.A.exns) Lwt_result.t
  val to_string : t -> string
end = struct
  type metadata_t = (string * OBus_value.V.single) list
  type t = [ `TrackInfo of track_info | `Length of int64 ]

  let artist md =
    let artist = List.assoc_opt "xesam:artist" md in
    let artist' =
      Option.map OBus_value.(C.cast_single @@ C.array C.basic_string) artist
    in
    let artist'' = Option.map (String.concat ", ") artist' in
    Option.value artist'' ~default:"NOT FOUND"

  let title md =
    let title = List.assoc_opt "xesam:title" md in
    let title' = Option.map OBus_value.(C.cast_single C.basic_string) title in
    Option.value title' ~default:"NOT FOUND"

  let length md =
    let length = List.assoc_opt "mpris:length" md in
    (* NOTE: For some reason OBus thinks this is a (DBus type) uint64, but the spec says it is signed
       Even `dbus-send` says this is uint64 *)
    let length' = Option.map OBus_value.(C.cast_single C.basic_uint64) length in
    Option.value length' ~default:0L

  let art_url md =
    let au = List.assoc_opt "mpris:artUrl" md in
    let au' = Option.map OBus_value.(C.cast_single C.basic_string) au in
    Option.value au' ~default:"NOT FOUND"

  let make_track_info md = `TrackInfo { title = title md; artist = artist md }
  let make_length md = `Length (length md)
  let make_art_url md = ArtUrl.A.from_url @@ art_url md

  let to_string = function
    | `TrackInfo ti -> ti.title ^ ti.artist
    | `Length l -> microsecond_to_minsec l
    | `ArtUrl s -> s
end

module Volume : sig
  type t = private float

  val make : float -> t
  val to_string : t -> string
end = struct
  type t = float

  let make f = f
  let to_string t = Printf.sprintf "%5.1f" (100. *. t)
end

module PlaybackStatus : sig
  type t = private Playing | Paused | Stopped

  val make : string -> t
  val to_string : t -> string
  val if_playing : t -> (unit -> unit Lwt.t) -> unit Lwt.t
end = struct
  type t = Playing | Paused | Stopped

  let make = function
    | "Playing" -> Playing
    | "Paused" -> Paused
    | "Stopped" -> Stopped
    | _ -> failwith "playback_status_of_string: Invalid PlaybackStatus"

  let to_string = function
    | Playing -> "Playing"
    | Paused -> "Paused"
    | Stopped -> "Stopped"

  let if_playing playback_status f =
    match playback_status with
    | Paused | Stopped -> Lwt.return_unit
    | Playing -> f ()
end

module Position : sig
  type t = int64

  val make : int64 -> t
  val to_string : t -> string
end = struct
  type t = int64

  let make f = f
  let to_string = microsecond_to_minsec
end

module Rate : sig
  type t = float

  val make : float -> t
  val to_string : t -> string
end = struct
  type t = float

  let make f = f
  let to_string t = Printf.sprintf "x%3.1f" t
end
