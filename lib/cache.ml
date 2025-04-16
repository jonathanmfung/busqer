module type FILECACHE = sig
  type key_t = string
  type t

  val dump : t -> (unit, exn) Lwt_result.t
  val from : key_t -> (string, exn) Lwt_result.t
  val abs_path : key_t -> string
end

module type FILESERIALIZETYPE = sig
  type key_t = string
  type t = { key : key_t; data : string }

  val of_key : key_t -> t
  val filename : key_t -> string
end

module type CONFIGTYPE = sig
  val cache_dir : string
end

module Make (C : CONFIGTYPE) (Fs : FILESERIALIZETYPE) :
  FILECACHE with type t = Fs.t = struct
  include Fs

  let abs_path k = Filename.concat C.cache_dir (Fs.filename k)

  let cache_dir_ensure cd =
    if not (Sys.file_exists cd && Sys.is_directory cd) then Sys.mkdir cd 0o700

  (* TODO: Make absoulte path *)
  let dump t =
    cache_dir_ensure C.cache_dir;
    let fname = abs_path t.key in
    let f ch = Lwt_io.write ch t.data in
    let ( let* ) = Lwt.bind in
    let* () = Log.err "FILECACHE Dumped to %s" fname in
    Lwt_result.catch @@ fun () -> Lwt_io.with_file ~mode:Lwt_io.Output fname f

  let from k =
    cache_dir_ensure C.cache_dir;
    let fname = abs_path k in
    let f ch = Lwt_io.read ch in
    Lwt_result.catch @@ fun () -> Lwt_io.with_file ~mode:Lwt_io.input fname f
end
