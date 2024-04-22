open Rwo_eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let cwd = Eio.Stdenv.cwd env in
  let clock = Eio.Stdenv.clock env in

  save ~cwd ~path:"test.txt" ~content:"This is only a test.";

  let contents = file_contents ~cwd ~filename:"test.txt" in
  print_endline contents;

  uppercase_file ~cwd ~filename:"test.txt";

  let lines = count_lines ~cwd ~filename:"test.txt" in
  print_int lines;
  print_newline ();

  let delayer = Delayer.create ~delay:1.0 in

  let first_promise =
    Delayer.schedule ~sw ~clock delayer (fun () ->
        print_endline "Running my first action.";
        let my_result = 5 in
        Eio.Promise.create_resolved my_result)
  in

  print_endline "Waiting for the first promise to complete.";
  let my_result = Eio.Promise.await first_promise in
  print_endline "First promise completed.";
  print_int my_result;
  print_newline ();

  print_endline "Schedule second task";
  let second_promise =
    Delayer.schedule ~sw ~clock delayer (fun () ->
        print_endline "Running my second action.";
        let my_result = 10 in
        Eio.Promise.create_resolved my_result)
  in

  print_endline "Sleeping for 2 seconds.";
  Eio.Time.sleep clock 2.0;

  (* Promise will resolve ~3 seconds after we continue after sleeping. *)
  print_endline "Waiting for the second promise to complete.";
  Eio.Promise.await second_promise |> print_int;
  print_newline ();

  Eio.Fiber.fork ~sw (fun () -> run ~net);
  Eio.Fiber.fork ~sw (fun () -> run_client ~net ~port:8080);
  Eio.Fiber.fork ~sw (fun () -> run_client ~net ~port:8080);
  Eio.Fiber.fork ~sw (fun () -> run_client ~net ~port:8080);
  Eio.Fiber.fork ~sw (fun () -> run_client ~net ~port:8080);
  Eio.Fiber.fork ~sw (fun () -> run_client ~net ~port:8080);

  Eio.Fiber.fork ~sw (fun () -> improved_run ~net ~uppercase:true ~port:8081);
  Eio.Fiber.fork ~sw (fun () -> run_client ~net ~port:8081);

  let duckduckgo_result = get_definition_from_json ~net "ocaml" in
  print_result duckduckgo_result;

  search_and_print ~net [ "ocaml"; "reasonml"; "graphql"; "reason-react" ];
  search_and_print_v2 ~net [ "ocaml"; "reasonml"; "graphql"; "reason-react" ];

  ()
