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
      let cache_dir = Filename.concat "/home/jonat/.cache" "busqer"
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
            | Result.Error e2 ->
                failwith @@ Printexc.to_string e2 ^ "\n" ^ Printexc.to_string e1
            ))

  let to_cache t =
    (* TODO: Consider not dumping if file already exists *)
    JpgCache.dump t

  let abs_path t = JpgCache.abs_path t.key
  let _to_pixbuf = ()
end

(* From GPointer *)
(* Consider using BigArray:
   type pb_bigarray = ((int * int * int), Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t
*)

type pb_array = (int * int * int) Array.t Array.t

let pixbuf_to_array (pb : GdkPixbuf.pixbuf) : pb_array =
  let open GdkPixbuf in
  (* https://docs.gtk.org/gdk-pixbuf/class.Pixbuf.html#image-data *)
  (* NOTE: Colorspace is only RGB *)
  let bits_per_sample = get_bits_per_sample pb in
  assert (bits_per_sample == 8);
  Printf.printf "Bits_per_sample: %i\n" bits_per_sample;

  (* TODO: Handle n_channels/alpha
     I think a sequence would be easier to handle alpha *)
  let n_channels = get_n_channels pb in
  Printf.printf "Num channels: %i\n" n_channels;
  let has_alpha = get_has_alpha pb in
  if has_alpha then failwith "Pixbuf has alpha channel";
  let height_px = get_height pb in
  let width_px = get_width pb in

  (* Last row may not be full-width (from gdk-pixbuf docs) *)
  let last_row_byte_width =
    width_px * (((n_channels * bits_per_sample) + 7) / 8)
  in
  let pixels = GdkPixbuf.get_pixels pb in

  let px_val_at x y =
    let x_b = x * n_channels * bits_per_sample / 8 in
    if y = height_px - 1 && last_row_byte_width - 1 < x_b then
      invalid_arg
        (Printf.sprintf "Attempting to access %i which is past %i in last row."
           x_b last_row_byte_width);
    let byte_lin = (y * get_rowstride pb) + x_b in
    Gpointer.
      ( get_byte pixels ~pos:byte_lin,
        get_byte pixels ~pos:(byte_lin + 1),
        get_byte pixels ~pos:(byte_lin + 2) )
  in
  Array.init_matrix width_px height_px px_val_at

(*
Mean shift clustering
Init cluster starts at first pixel's color
If some Points' color still not in a cluster, start new cluster

*)
