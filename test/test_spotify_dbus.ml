open OUnit2

let pixbuf_tests =
  let pb = GdkPixbuf.from_file "./data/black_10x10.jpg" in
  let pixels = GdkPixbuf.get_pixels pb in
  let first_red = Gpointer.get_byte pixels ~pos:1 in
  let first_green = Gpointer.get_byte pixels ~pos:2 in
  let first_blue = Gpointer.get_byte pixels ~pos:3 in
  let eq = assert_equal ~msg:"int value" ~printer:Int.to_string in
  "PixBuf"
  >::: [
         ("R" >:: fun _ -> eq first_red 0);
         ("G" >:: fun _ -> eq first_green 0);
         ("B" >:: fun _ -> eq first_blue 0);
       ]

let arr_to_string =
  Array.fold_left
      (fun acc (r, g, b) ->
        acc ^ Format.sprintf "(%i,%i,%i) " r g b)
      ""
let arr2_to_string = Array.fold_left (fun acc a -> acc ^ Format.sprintf "[%s]" (arr_to_string a)) ""

let pixbuf_to_array_test =
  let pb = GdkPixbuf.from_file "./data/black_10x10.jpg" in
  let result = Spotify_dbus.ArtUrl.pixbuf_to_array pb in
  let expected = Array.make_matrix 10 10 (0, 0, 0) in
  "pixbuf_to_array" >:: fun _ ->
  assert_equal ~msg:"RGB Array2" ~printer:arr2_to_string expected result

let tests = test_list [ pixbuf_tests; pixbuf_to_array_test ]
let _ = run_test_tt_main tests
