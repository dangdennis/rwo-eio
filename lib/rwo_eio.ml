(* Code translated from RWO Async to eio. *)
(* See bin/main.ml for usage. *)

(* Async Basics Section *)

(* Use of eio typically means we call Eio_main.run at the root of our application. *)
(* We then pass explicit capabilities to our functions, such at cwd (current working directory), net,  *)
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

(* Ivars and Upon Section *)

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

(* Example: An Echo Server Section *)

(* Unsure if Flow.copy handles pushback like the Async example *)
(* TODO: Ask community for help. *)
let copy_blocks src dst = Eio.Flow.copy src dst

let run ~net : unit =
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let socket = Eio.Net.listen ~backlog:5 ~sw net addr in
  let handle_client flow addr =
    Eio.traceln "Server: got connection from client %a" Eio.Net.Sockaddr.pp addr;
    let buffer = Eio.Buf_read.of_flow flow ~max_size:1024 in
    (* Read all data until first newline *)
    let client_msg = Eio.Buf_read.line buffer in
    Eio.traceln "Server: received: %S" client_msg;
    Eio.Flow.copy_string client_msg flow
  in
  Eio.Net.run_server socket handle_client ~on_error:(fun _ -> Eio.traceln "Server: error")

(* client to connect and send message  *)
let run_client ~net =
  Eio.Switch.run ~name:"client" @@ fun sw ->
  Eio.traceln "Client: connecting to server";
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Buf_write.with_flow flow (fun to_server -> Eio.Buf_write.string to_server "Hello from client 1\n");
  Eio.traceln "Client: received %S" (Eio.Flow.read_all flow)
