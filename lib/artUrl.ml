module Jpg : Cache.FILESERIALIZETYPE = struct
  type key_t = string
  type t = { key : key_t; data : string }

  (* TODO: consider option for uninitialized data *)
  let of_key key = { key; data = "" }
  let filename k = k ^ ".jpg"
end

module JpgCache =
  Cache.Make
    (struct
      (* TODO: This cache dir stuff might be more application-level *)
      let cache_dir = Filename.concat "/home/jonat/.cache" "spotify_dbus"
    end)
    (Jpg)

module A : sig
  type t
  type exns = exn list

  val from_url : string -> (t, exns) Lwt_result.t
  val to_cache : t -> (unit, exn) Lwt_result.t
  val abs_path : t -> string
end = struct
  type exns = exn list

  include Jpg

  let fetch uri : string Lwt.t =
    let ( let* ) = Lwt.bind in
    let* _resp, body = Cohttp_lwt_unix.Client.get uri in
    Cohttp_lwt.Body.to_string body

  let from_uri_fetch uri : ('a, 'b) Lwt_result.t = Lwt_result.ok @@ fetch uri

  let id_of_uri uri : (key_t, 'a) Lwt_result.t =
    let p = uri |> Uri.canonicalize |> Uri.path in
    let ps = String.split_on_char '/' p in
    match List.nth_opt ps 2 with
    | Some x -> Lwt_result.return (Jpg.of_key x).key
    | None -> Lwt_result.fail (Failure "id_of_uri: Improper URI path")

  let from_cache uri = Lwt_result.bind (id_of_uri uri) JpgCache.from

  let from_url url =
    let ( let* ) = Lwt.bind in
    let uri = Uri.of_string url in
    let* id = id_of_uri uri in
    match id with
    | Result.Error e -> Lwt_result.fail [ e ]
    | Result.Ok key -> (
        let* c = from_cache uri in
        match c with
        | Result.Ok x ->
            let* () = Log.err "ArtUrl from Cache" in
            Lwt_result.return { data = x; key }
        | Result.Error e1 -> (
            let* uf = from_uri_fetch uri in
            match uf with
            | Result.Ok y ->
                let* () = Log.err "ArtUrl from Url Fetch" in
                Lwt_result.return { data = y; key }
            | Result.Error e2 -> failwith @@ Printexc.to_string e2 ^ "\n" ^ Printexc.to_string e1))

  let to_cache t =
    (* TODO: Consider not dumping if file already exists *)
    JpgCache.dump t

  let abs_path t = JpgCache.abs_path t.key
  let _to_pixbuf = ()
end
