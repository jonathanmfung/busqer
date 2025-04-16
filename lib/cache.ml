module type FILECACHE = sig
  type t
  type key

  val dump : t -> (unit, exn) Lwt_result.t
  val from : key -> (string, exn) Lwt_result.t
end

module type FILESERIALIZETYPE = sig
  type v
  type key

  val key : v -> key
  val filename : key -> string
  val to_data : v -> string
end

module type CONFIGTYPE = sig
  val cache_dir : string
end

module Make (C : CONFIGTYPE) (Fs : FILESERIALIZETYPE) :
  FILECACHE with type t = Fs.v with type key = Fs.key = struct
  type t = Fs.v
  type key = Fs.key

  let abs_path k = Filename.concat C.cache_dir (Fs.filename k)

  let cache_dir_ensure cd =
    if not (Sys.file_exists cd && Sys.is_directory cd) then Sys.mkdir cd 0o700

  (* TODO: Make absoulte path *)
  let dump t =
    cache_dir_ensure C.cache_dir;
    let fname = abs_path (Fs.key t) in
    let f ch = Lwt_io.write ch (Fs.to_data t) in
    Lwt_result.catch @@ fun () -> Lwt_io.with_file ~mode:Lwt_io.Output fname f

  let from k =
    cache_dir_ensure C.cache_dir;
    let fname = abs_path k in
    let f ch = Lwt_io.read ch in
    Lwt_result.catch @@ fun () -> Lwt_io.with_file ~mode:Lwt_io.input fname f
end

module StringCache =
  (* TODO: Just for testing *)
    Make
      (struct
      let cache_dir = "/home/jonat/.cache/spotify_dbus"
    end)
    (struct
      type v = string
      type key = string

      let key t = String.uppercase_ascii t
      let filename k = k ^ "_filename.txt"
      let to_data t = "DATA: " ^ String.lowercase_ascii t
    end)
