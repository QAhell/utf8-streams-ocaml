let assertTrue condition message =
  assert (condition || (print_endline message ; condition)) ;;

assertTrue (Utf8_streams_ocaml.times_two 2 = 4) "Two times two should be four!" ;;
