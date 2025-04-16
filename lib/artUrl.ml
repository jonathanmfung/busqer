module A : sig
  type t
  type exns = exn list

  val from_url : string -> (t, exns) Lwt_result.t
  val to_cache : t -> (unit, exn) Lwt_result.t
end = struct
  type t = { id : string; data : string (* jpeg as string *) }
  type exns = exn list

  open Cohttp

  let fetch uri : string Lwt.t =
    let ( let* ) = Lwt.bind in
    let* resp, body = Cohttp_lwt_unix.Client.get uri in

    let code = resp |> Response.status |> Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    let body_s = Cohttp_lwt.Body.to_string body in
    let* () =
      Lwt.map
        (fun b -> Printf.printf "Body of length: %d\n" (String.length b))
        body_s
    in
    body_s

  let from_uri_fetch uri : ('a, 'b) Lwt_result.t = Lwt_result.ok @@ fetch uri

  let id_of_uri uri =
    let p = uri |> Uri.canonicalize |> Uri.path in
    let ps = String.split_on_char '/' p in
    match List.nth_opt ps 2 with
    | Some x -> Lwt_result.return x
    | None -> Lwt_result.fail (Failure "id_of_uri: Improper URI path")

  module JpgCache =
    Cache.Make
      (struct
        (* TODO: This cache dir stuff might be more application-level *)
        let cache_dir = Filename.concat "/home/jonat/.cache" "spotify_dbus"
      end)
      (struct
        type v = t
        type key = string (* last elem of artUrl path *)

        let key t = t.id
        let filename k = k ^ ".jpg"
        let to_data t = t.data
      end)

  let from_cache uri = Lwt_result.bind (id_of_uri uri) JpgCache.from

  let from_url _url =
    let ( let* ) = Lwt.bind in
    let url =
      "https://i.scdn.co/image/ab67616d0000b27325f8b0dfb1d5619234098cad"
    in
    let uri = Uri.of_string url in
    let* id = id_of_uri uri in
    match id with
    | Result.Error e -> Lwt_result.fail [ e ]
    | Result.Ok id -> (
        let* c = from_cache uri in
        match c with
        | Result.Ok x ->
            let* () = Log.out "From Cache" in
            Lwt_result.return { data = x; id }
        | Result.Error e1 -> (
            let* uf = from_uri_fetch uri in
            match uf with
            | Result.Ok y ->
                let* () = Log.out "From Url Fetch" in
                Lwt_result.return { data = y; id }
            | Result.Error e2 -> Lwt_result.fail [ e2; e1 ]))

  let to_cache t = JpgCache.dump t
  let _to_pixbuf = ()
end
