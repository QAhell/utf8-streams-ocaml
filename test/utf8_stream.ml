open Utf8_stream

let assertTrue condition message =
  assert (condition || (print_endline message ; condition)) ;;

module Test =
struct
  let test_position_at_start_of_input () =
    let r = position_at_start_of_input () in
    assertTrue (r.line = 0) "position_at_start_of_input should initialize line to 0" ;
    assertTrue (r.column = 0) "position_at_start_of_input should initialize column to 0" ;
    assertTrue (r.position = 0) "position_at_start_of_input should initialize position to 0"
  let test_string_input () =
    let s0 = String_input.of_string "aB$\x05" in
    (match String_input.get s0 with | Some (c0, s1) ->
    (match String_input.get s1 with | Some (c1, s2) ->
    (match String_input.get s2 with | Some (c2, s3) ->
    (match String_input.get s3 with | Some (c3, s4) ->
    (match String_input.get s4 with | None ->
        assertTrue (c0 = 'a')
          "Character 0 of string input \"aB$\\x05\" should be 'a'" ;
        assertTrue (c1 = 'B')
          "Character 1 of string input \"aB$\\x05\" should be 'B'" ;
        assertTrue (c2 = '$')
          "Character 2 of string input \"aB$\\x05\" should be '$'" ;
        assertTrue (Char.code c3 = 5)
          "Character 3 of string input \"aB$\\x05\" should be '\\x05'"
      | Some _ ->
             assertTrue false "String input \"aB$\\x05\" should have 4 chars, not 5")
      | _ -> assertTrue false "String input \"aB$\\x05\" should have 4 chars, not 3")
      | _ -> assertTrue false "String input \"aB$\\x05\" should have 4 chars, not 2")
      | _ -> assertTrue false "String input \"aB$\\x05\" should have 4 chars, not 1")
      | _ -> assertTrue false "String input \"aB$\\x05\" should have 4 chars, not 0")
  let test_decoded_string_input () =
    let s0 =
      Decoded_string_input.of_string "\xD9\xA9\x28\xCD\xA1\xE0\xB9\x8F" in
    (match Decoded_string_input.get s0 with | Some (c0, s1) ->
    (match Decoded_string_input.get s1 with | Some (c1, s2) ->
    (match Decoded_string_input.get s2 with | Some (c2, s3) ->
    (match Decoded_string_input.get s3 with | Some (c3, s4) ->
    (match Decoded_string_input.get s4 with | None ->
        assertTrue (c0 = 1641) "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" at index 0 should be 1641" ;
        assertTrue (c1 = 40) "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" at index 1 should be 40" ;
        assertTrue (c2 = 865) "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" at index 2 should be 865" ;
        assertTrue (c3 = 3663) "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" at index 3 should be 3663"
      | Some _ -> assertTrue false "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" should have 4 code points, not 5")
      | _ -> assertTrue false "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" should have 4 code points, not 3")
      | _ -> assertTrue false "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" should have 4 code points, not 2")
      | _ -> assertTrue false "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" should have 4 code points, not 1")
      | _ -> assertTrue false "Decoded string \"\\xD9\\xA9\\x28\\xCD\\xA1\\xE0\\xB9\\x8F\" should have 4 code points, not 0")
  let test_string_output () =
    let e0 = String_output.empty () in
    let e1 = String_output.put e0 'f' in
    let e2 = String_output.put_str e1 "oo" in
    assert ("" = String_output.to_string e0) ;
    assert ("f" = String_output.to_string e1) ;
    assert ("foo" = String_output.to_string e2)
  let test_int_list_input () =
    let xs0 = [1;2;3] in
    (match Code_point_list_input.get xs0 with
      | Some (1, xs1) ->
    (match Code_point_list_input.get xs1 with
      | Some (2, xs2) ->
    (match Code_point_list_input.get xs2 with
      | Some (3, xs3) ->
    (match Code_point_list_input.get xs3 with
      | None -> ()
      | _ -> assert false)
      | _ -> assert false)
      | _ -> assert false)
      | _ -> assert false)
  let flip f x y = f y x
  let test_int_list_output () =
    let xs = []
        |> flip Code_point_list_output.put 1
        |> flip Code_point_list_output.put 2
        |> flip Code_point_list_output.put 3 in
    assert (xs = [3;2;1])
  let test_bytes_of_code_point () =
    assert (bytes_of_code_point 42 = [42]) ;
    assert (bytes_of_code_point 1641 = [0xD9; 0xA9]) ;
    assert (bytes_of_code_point 865 = [0xCD; 0xA1])
  let test_code_point_of_bytes () =
    assert (try ignore (code_point_of_bytes "ab") ; false with _ -> true) ;
    assert (try code_point_of_bytes "\xD9\xA9" = 1641 with _ -> false) ;
    assert (try code_point_of_bytes "\xCD\xA1" = 865 with _ -> false)
  let test_string_of_code_points () =
    assert (string_of_code_points [1641; 40; 865] = "\xD9\xA9\x28\xCD\xA1")

  let test_buffers () =
    let output_buffer = Buffer_output.start_at_position 0 3 3 in
    let input_buffer = output_buffer
          |> flip Buffer_output.put (Char.code 'a')
          |> flip Buffer_output.put (Char.code '\n')
          |> (fun s -> Buffer_output.put_with_length s (Char.code '&') 5)
          |> Buffer_output.input_of in
    match Buffer_input.get_with_position input_buffer with
      | Some ({ input = a; line = l; column = c; position = p }, input_buffer) ->
          assert (a = Char.code 'a') ;
          assert (l = 0) ;
          assert (c = 3) ;
          assert (p = 3) ;
    (match Buffer_input.get_with_position input_buffer with
      | Some ({ input = n; line = l; column = c; position = p }, input_buffer) ->
          assert (n = Char.code '\n') ;
          assert (l = 0) ;
          assert (c = 4) ;
          assert (p = 4) ;
    (match Buffer_input.get_with_position input_buffer with
      | Some ({ input = b; line = l; column = c; position = p }, input_buffer) ->
          assert (b = Char.code '&') ;
          assert (l = 1) ;
          assert (c = 0) ;
          assert (p = 5) ;
          assert (Buffer_input.line input_buffer = 1) ;
          assert (Buffer_input.column input_buffer = 5) ;
          assert (Buffer_input.position input_buffer = 10) ;
    (match Buffer_input.get_with_position input_buffer with
      | None -> ()
      | Some _ -> assert false)
      | _ -> assert false)
      | _ -> assert false)
      | _ -> assert false ;;

  let test () =
    test_position_at_start_of_input () ;
    test_string_input () ;
    test_decoded_string_input () ;
    test_string_output () ;
    test_int_list_input () ;
    test_int_list_output () ;
    test_bytes_of_code_point () ;
    test_code_point_of_bytes () ;
    test_string_of_code_points () ;
    test_buffers ()
end ;;

Test.test() ;;
