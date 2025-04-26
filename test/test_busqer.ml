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
    (fun acc (r, g, b) -> acc ^ Format.sprintf "(%i,%i,%i) " r g b)
    ""

let arr2_to_string =
  Array.fold_left
    (fun acc a -> acc ^ Format.sprintf "[%s]" (arr_to_string a))
    ""

let pixbuf_to_array_test =
  let pb = GdkPixbuf.from_file "./data/black_10x10.jpg" in
  let result = Busqer.ArtUrl.pixbuf_to_array pb in
  let expected = Array.make_matrix 10 10 (0, 0, 0) in
  "pixbuf_to_array" >:: fun _ ->
  assert_equal ~msg:"RGB Array2" ~printer:arr2_to_string expected result

let int_to_bits_test =
  let eq =
    assert_equal ~msg:"bool list value"
      ~printer:(List.fold_left (fun acc a -> acc ^ Format.sprintf "%b " a) "")
  in
  let open Spotify_dbus.Octree in
  "int_to_bits"
  >::: [
         ( "0" >:: fun _ ->
           eq
             (List.of_seq @@ int_to_bits 0)
             [ false; false; false; false; false; false; false; false ] );
         ( "2" >:: fun _ ->
           eq
             (List.of_seq @@ int_to_bits 2)
             [ false; false; false; false; false; false; true; false ] );
         ( "4" >:: fun _ ->
           eq
             (List.of_seq @@ int_to_bits 4)
             [ false; false; false; false; false; true; false; false ] );
         ( "9" >:: fun _ ->
           eq
             (List.of_seq @@ int_to_bits 9)
             [ false; false; false; false; true; false; false; true ] );
       ]

let rgb_gen : Spotify_dbus.Octree.rgb QCheck2.Gen.t =
  let rgb x y z = Spotify_dbus.Octree.Rgb (x, y, z) in
  QCheck2.Gen.(
    let u8 = 0 -- 255 in
    rgb <$> u8 <*> u8 <*> u8)

let rgb_print =
  let unrgb (Spotify_dbus.Octree.Rgb (x, y, z)) = (x, y, z) in
  QCheck2.Print.(contramap unrgb @@ triple int int int)

let octree_prop_tests =
  let open Spotify_dbus.Octree in
  (* let eq = assert_equal ~msg:"octree value" ~printer:to_string in *)
  "OctreePropTests"
  >::: List.map QCheck_ounit.to_ounit2_test
         [
           QCheck2.(
             Test.make ~count:100 ~name:"octree_insert_duplicate"
               ~print:rgb_print rgb_gen (fun a ->
                 equal (insert (insert empty a) a) (insert (insert empty a) a)));
           QCheck2.(
             Test.make ~count:100 ~name:"octree_insert_is_commutative"
               ~print:Print.(pair rgb_print rgb_print)
               (Gen.pair rgb_gen rgb_gen)
               (fun (a, b) ->
                 equal (insert (insert empty a) b) (insert (insert empty b) a)));
         ]

let tests =
  test_list
    [ pixbuf_tests; pixbuf_to_array_test; int_to_bits_test; octree_prop_tests ]

let _ = run_test_tt_main tests
