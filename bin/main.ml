open Rwo_eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  let clock = Eio.Stdenv.clock env in

  save ~cwd ~path:"test.txt" ~content:"This is only a test.";

  let contents = file_contents ~cwd ~filename:"test.txt" in
  print_endline contents;

  uppercase_file ~cwd ~filename:"test.txt";

  let lines = count_lines ~cwd ~filename:"test.txt" in
  print_int lines;

  let delayer = Delayer.create ~delay:5.0 in

  let first_promise =
    Delayer.schedule ~sw ~clock delayer (fun () ->
        print_endline "Running my first action after 5 seconds.";
        let my_result = 5 in
        Eio.Promise.create_resolved my_result)
  in

  let second_promise =
    Delayer.schedule ~sw ~clock delayer (fun () ->
        print_endline "Running my second action after 5 seconds.";
        let my_result = 10 in
        Eio.Promise.create_resolved my_result)
  in

  print_endline "Waiting for the first promise to complete.";
  let my_result = Eio.Promise.await first_promise in
  print_endline "First promise completed.";
  print_int my_result;
  print_newline ();

  print_endline "Waiting for the second promise to complete.";
  Eio.Promise.await second_promise |> print_int;
  print_newline ();

  ()
