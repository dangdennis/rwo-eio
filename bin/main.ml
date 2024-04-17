open Rwo_eio

let () =
  save "test.txt" "This is only a test.";

  let contents = file_contents "test.txt" in
  print_endline contents;

  uppercase_file "test.txt";

  let lines = count_lines "test.txt" in
  print_int lines;

  ()