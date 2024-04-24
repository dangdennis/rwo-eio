(* Code translated from RWO Async to eio. *)
(* See bin/main.ml for usage. *)

(* Async Basics *)

(* Use of eio typically means we call Eio_main.run at the root of our application. *)
(* This is equivalent to Lwt_main.run and Async.Scheduler.go *)
(* bin/main.ml will run its own eio scheduler to run the rest of our examples. *)
(* let example_eio_app () =
   Eio_main.run (fun env ->
       print_endline "Initialize the eio scheduler and do nothing.";
       let _net = Eio.Stdenv.net env in
       let _cwd = Eio.Stdenv.cwd env in
       let _clock = Eio.Stdenv.clock env in
       ()) *)

(* We then pass explicit capabilities to our functions, such as cwd (current working directory), net,  *)
let save ~cwd ~path ~content : unit =
  let ( / ) = Eio.Path.( / ) in
  Eio.Path.save ~create:(`Or_truncate 0o777) (cwd / path) content

let file_contents ~cwd ~(filename : string) : string =
  let ( / ) = Eio.Path.( / ) in
  Eio.Path.load (cwd / filename)

let uppercase_file ~cwd ~(filename : string) : unit =
  let ( / ) = Eio.Path.( / ) in
  let contents = Eio.Path.load (cwd / filename) in
  let uppercase = String.uppercase_ascii contents in
  (* `Or_truncate 0o777 allows overwrites to the file if it exists. Unix things. *)
  (* https://github.com/ocaml-multicore/eio/blob/c1c2d634dee8640a386e1343063ad820ae3fe4fd/lib_eio/fs.ml#L41 *)
  Eio.Path.save ~create:(`Or_truncate 0o777) (cwd / filename) uppercase

let count_lines ~cwd ~(filename : string) : int =
  let ( / ) = Eio.Path.( / ) in
  let contents = Eio.Path.load (cwd / filename) in
  let lines = String.split_on_char '\n' contents in
  List.length lines

(* Ivars and Upon *)

(* The closest type to async's ivar is eio's promise. *)
(* We provide a unit type because Eio.Promise.t requires a type parameter.  *)
let (_promise : unit Eio.Promise.t), _resolver = Eio.Promise.create ()

module type Delayer_intf = sig
  type t

  val create : delay:float -> t

  val schedule :
    sw:Eio.Switch.t ->
    clock:'a Eio.Time.clock ->
    t ->
    (unit -> 'b Eio.Promise.t) ->
    'b Eio.Promise.t
end

module Delayer : Delayer_intf = struct
  type t = { delay : float; jobs : (unit -> unit) Queue.t }

  let create ~(delay : float) : t = { delay; jobs = Queue.create () }

  let schedule ~sw ~clock (t : t) (f : unit -> 'a Eio.Promise.t) : 'a Eio.Promise.t =
    let promise, resolver = Eio.Promise.create () in

    Queue.add
      (fun () ->
        let x = f () |> Eio.Promise.await in
        let _ = Eio.Promise.resolve resolver x in
        ())
      t.jobs;

    Eio.Fiber.fork ~sw (fun () ->
        Eio.Time.sleep clock t.delay;
        if Queue.is_empty t.jobs then ()
        else
          let job = Queue.pop t.jobs in
          job ());

    promise
end

(* Example: An Echo Server *)

(* Unsure if Flow.copy handles pushback like the Async example *)
(* TODO: Ask community for help. *)
let copy_blocks src dst = Eio.Flow.copy src dst

(* Eio.Net.run_server will run forever and block the main thread, unless it runs in a separate Eio.Fiber *)
let run ~net : unit =
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let socket = Eio.Net.listen ~backlog:5 ~sw net addr in
  (* TODO: How to work with eio to create an intermediate buffer that copies from Flow reader to Flow writer *)
  let handle_client flow addr =
    Eio.traceln "Server: got connection from client %a" Eio.Net.Sockaddr.pp addr;
    let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in
    let msg = Eio.Buf_read.line reader in
    Eio.traceln "Server: received: %S" msg;
    Eio.Flow.copy_string msg flow
  in
  Eio.Net.run_server socket handle_client ~on_error:(fun _ -> Eio.traceln "Server: error")

(* client to connect to the server via TCP  *)
let run_client ~net ~port =
  Eio.Switch.run ~name:"client" @@ fun sw ->
  Eio.traceln "Client: connecting to server";
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Buf_write.with_flow flow (fun to_server ->
      Eio.Buf_write.string to_server "Hello from client 1\n");
  Eio.traceln "Client: received %S" (Eio.Flow.read_all flow)

(* Improving the Echo Server *)
let improved_run ~net ~uppercase ~port : unit =
  Eio.traceln "Running server with port %d and uppercase %b" port uppercase;
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let socket = Eio.Net.listen ~backlog:5 ~sw net addr in
  let handle_client flow addr =
    Eio.traceln "Server: got connection from client %a" Eio.Net.Sockaddr.pp addr;
    let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in
    let msg = Eio.Buf_read.line reader in
    let msg = if uppercase then String.uppercase_ascii msg else msg in
    Eio.traceln "Server: received: %S" msg;
    Eio.Flow.copy_string msg flow
  in
  Eio.Net.run_server socket handle_client ~on_error:(fun _ -> Eio.traceln "Server: error")

(* See usage in  *)
let server_cli () =
  print_endline "Running server as CLI";
  let open Command.Param in
  let command =
    Command.basic ~summary:"Start an echo server"
      (let uppercase_param =
         flag "-uppercase" (optional bool) ~doc:"Convert to uppercase before echoing back"
       in
       let port_param = flag "-port" (optional int) ~doc:"Port to listen on (default 8080)" in
       map (both uppercase_param port_param) ~f:(fun (uppercase, port) () ->
           Eio_main.run @@ fun env ->
           let net = Eio.Stdenv.net env in
           match (uppercase, port) with
           | Some uppercase, Some port -> improved_run ~net ~uppercase ~port
           | Some uppercase, None -> improved_run ~net ~uppercase ~port:8080
           | None, Some port -> improved_run ~net ~uppercase:false ~port
           | None, None -> improved_run ~net ~uppercase:false ~port:8080))
  in
  Command_unix.run command

(* Alternative server cli that uses the stdlib *)
let server_cli_simple () =
  let usage_msg = "Start an echo server" in
  let port = ref 8080 in
  let uppercase = ref false in
  let speclist =
    [
      ("-port", Arg.Set_int port, "Port to listen on (default 8080)");
      ("-uppercase", Arg.Set uppercase, "Convert to uppercase before echoing back");
    ]
  in
  let anon_fun _ = () in
  let () = Arg.parse speclist anon_fun usage_msg in
  Eio_main.run (fun env -> improved_run ~net:(Eio.Stdenv.net env) ~uppercase:!uppercase ~port:!port)

(* Example: Searching Definitions with DuckDuckGo *)

let query_uri query =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  Uri.add_query_param base_uri ("q", [ query ])

(* Parsing JSON Strings *)

let get_definition_from_json (json : string) : string option =
  (* If you're curious what the raw json looks like, uncomment below. *)
  (* print_endline "Parsing JSON string";
     print_endline json; *)
  match Yojson.Safe.from_string json with
  | `Assoc kv_list -> (
      let find key =
        match List.assoc key kv_list with
        | exception Not_found -> None
        | `String "" -> None
        | s -> Some (Yojson.Safe.to_string s)
      in
      match find "Abstract" with Some _ as x -> x | None -> find "Definition")
  | _ -> None

(* Executing an HTTP Client Query *)
(* eio and cohttp-eio do not use monadic error types, but we get exceptions with stacktraces! *)
let get_definition ~net word =
  Eio.traceln "Getting definition from DuckDuckGo";
  Eio.Switch.run @@ fun sw ->
  let client = Cohttp_eio.Client.make ~https:None net in
  let resp, body = Cohttp_eio.Client.get ~sw client (query_uri word) in
  if Http.Status.compare resp.status `OK = 0 then
    let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    (word, get_definition_from_json body)
  else (
    (* We'll raise an error for simplicity *)
    Eio.traceln "Unexpected HTTP status: %a" Http.Status.pp resp.status;
    raise (Failure "Unexpected HTTP status"))

let print_result (word, definition) =
  match definition with
  | Some def ->
      Eio.traceln "%s: %s\n\n" word (String.concat "\n" (Wrapper.wrap (Wrapper.make 70) def))
  | None -> Eio.traceln "%s: \nNo definition found\n\n" word

let search_and_print ~net words =
  words |> Eio.Fiber.List.map (fun word -> get_definition ~net word) |> List.iter print_result

(* Prints in parallel as opposed to the previous that collects all before printing *)
let search_and_print_in_parallel ~net words =
  words |> Eio.Fiber.List.iter (fun word -> get_definition ~net word |> print_result)

let search_cli () =
  let open Command.Param in
  let command =
    Command.basic ~summary:"Retrieve definitions from duckduckgo search engine"
      (let words_param = anon (sequence ("words" %: string)) in
       map words_param ~f:(fun words () ->
           Eio_main.run @@ fun env ->
           let net = Eio.Stdenv.net env in
           search_and_print ~net words))
  in
  Command_unix.run command

(* Exception Handling *)
(* With eio, we can use plain old try-catch and plain old exceptions *)

let maybe_raise () =
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () -> if Random.bool () then raise Exit else raise Not_found)

let handle_error () =
  try maybe_raise () with
  | Exit -> print_endline "Caught Exit exception"
  | exn -> Fmt.pr "Caught exception: %s" (Printexc.to_string exn)

(* Monitors *)
(* eio does not have an equivalent of Async's monitors because there's no need.  *)
(* You can fork and catch exceptions however you want, and retry failed fibers. *)
(* Below is a simple example without fibers. *)
let monitor f =
  let rec loop count =
    if count >= 3 then Eio.traceln "Failed after 3 attempts"
    else
      try f ()
      with exn ->
        Eio.traceln "Caught exception: %s" (Printexc.to_string exn);
        loop (count + 1)
  in
  loop 0

let blow_up () = raise (Failure "Monitor error")

(* Example: Handling Exceptions with DuckDuckGo *)

let improved_query_uri ~server query =
  let base_uri = Uri.of_string (String.concat "" [ "http://"; server; "/?format=json" ]) in
  Uri.add_query_param base_uri ("q", [ query ])

let improved_get_definition ~net ~server word =
  Eio.traceln "Getting definition from DuckDuckGo";
  Eio.Switch.run @@ fun sw ->
  let client = Cohttp_eio.Client.make ~https:None net in
  let resp, body = Cohttp_eio.Client.get ~sw client (improved_query_uri ~server word) in
  if Http.Status.compare resp.status `OK = 0 then
    let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    (word, get_definition_from_json body)
  else (
    (* We'll raise an error for simplicity *)
    Eio.traceln "Unexpected HTTP status: %a" Http.Status.pp resp.status;
    raise (Failure "Unexpected HTTP status"))

let improved_search_and_print ~net ~server words =
  words
  |> Eio.Fiber.List.iter (fun word ->
         try improved_get_definition ~net ~server word |> print_result
         with exn -> Eio.traceln "Caught exception: %s" (Printexc.to_string exn))

let improved_search_cli () =
  let open Command.Param in
  let command =
    Command.basic ~summary:"Retrieve definitions from duckduckgo search engine"
      (let words_param = anon (sequence ("words" %: string)) in
       let servers_params = flag "-servers" (listed string) ~doc:"Server to query" in
       map (both words_param servers_params) ~f:(fun (words, servers) () ->
           Eio_main.run @@ fun env ->
           let net = Eio.Stdenv.net env in
           List.iter (fun server -> improved_search_and_print ~net ~server words) servers))
  in
  Command_unix.run command

(* Timeouts, Cancellation, and Choices *)

(* eio doesn't have a convenience method similar to Deferred.both. *)
(* But we can replicate that! *)
let string_and_float ~clock =
  Eio.Switch.run @@ fun sw ->
  let result1 =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Time.sleep clock 1.0;
        "A")
  in
  let result2 =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Time.sleep clock 2.0;
        3.14)
  in
  (Eio.Promise.await_exn result1, Eio.Promise.await_exn result2)

(* warning: `first` does not guarantee that exactly one of two actions is taken. *)
(* https://github.com/ocaml-multicore/eio?tab=readme-ov-file#racing *)
let improved_get_definition_with_timeout ~clock ~net ~server ~timeout word =
  (* We could use Eio.Time.with_timeout if we don't need the results. *)
  Eio.Fiber.first
    (fun () -> improved_get_definition ~net ~server word)
    (fun () ->
      Eio.Time.sleep clock timeout;
      (word, None))

(* todo: figure out how if cohttp eio has an interrupt option and eio has an *)
(* equivalent of Async's choose and choice *)

(* Working with System Threads *)

(* https://github.com/ocaml-multicore/eio?tab=readme-ov-file#unix-and-system-threads *)
let def () =
  Eio_unix.run_in_systhread (fun () ->
      let open Base in
      List.range 0 10)

(* eio doesn't yet provide a Clock.every equivalent. *)
let every ~clock ~sw ~stop period f =
  let rec loop () =
    if Eio.Promise.is_resolved stop then ()
    else (
      Eio.Time.sleep clock period;
      f ();
      loop ())
  in
  Eio.Fiber.fork ~sw (fun () -> loop ())
