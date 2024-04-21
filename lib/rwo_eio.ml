(* Code translated from RWO Async to eio. *)
(* See bin/main.ml for usage. *)

(* Async Basics *)

(* Use of eio typically means we call Eio_main.run at the root of our application. *)
(* This is equivalent to Lwt_main.run and Async.Scheduler.go *)
(* bin/main.ml will run its own eio scheduler to run the rest of our examples. *)
let () =
  Eio_main.run (fun env ->
      print_endline "Initialize the eio scheduler and do nothing.";
      let _net = Eio.Stdenv.net env in
      let _cwd = Eio.Stdenv.cwd env in
      let _clock = Eio.Stdenv.clock env in
      ())

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
  val schedule : sw:Eio.Switch.t -> clock:'a Eio.Time.clock -> t -> (unit -> 'b Eio.Promise.t) -> 'b Eio.Promise.t
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
  Eio.Buf_write.with_flow flow (fun to_server -> Eio.Buf_write.string to_server "Hello from client 1\n");
  Eio.traceln "Client: received %S" (Eio.Flow.read_all flow)

(* Improving the Echo Server *)
let improved_run ~net ~uppercase ~port : unit =
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

(* Example: Searching Definitions with DuckDuckGo *)

let query_uri query =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  Uri.add_query_param base_uri ("q", [ query ])

(* Parsing JSON Strings *)

let get_definition_from_json (json : string) : string option =
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

(* let () = Logs.set_reporter (Logs_fmt.reporter ())

   and () =
     (* The eio backend does not leverage domains yet, but might in the near future *)
     Logs_threaded.enable () *)

(* Executing an HTTP Client Query *)
(* eio and cohttp-eio do not use monadic error types, but we get exceptions with stacktraces! *)
let get_definition_from_json ~net word =
  Eio.traceln "Getting definition from DuckDuckGo";
  Eio.Switch.run @@ fun sw ->
  let client = Cohttp_eio.Client.make ~https:None net in
  let resp, body = Cohttp_eio.Client.get ~sw client (query_uri word) in
  if Http.Status.compare resp.status `OK = 0 then (
    let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    (word, get_definition_from_json body))
  else (
    (* We'll raise an error for simplicity *)
    Eio.traceln "Unexpected HTTP status: %a" Http.Status.pp resp.status;
    raise (Failure "Unexpected HTTP status"))

let print_result (word, definition) =
  match definition with
  | Some def -> Eio.traceln "%s: %s\n\n" word def
  | None -> Eio.traceln "%s: \nNo definition found\n\n" word
