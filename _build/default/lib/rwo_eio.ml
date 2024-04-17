(* Code translated from RWO Async to eio *)

(* Async Basics Section *)

let save path content : unit =
  Eio_main.run @@ fun env ->
  let ( / ) = Eio.Path.( / ) in
  let cwd = Eio.Stdenv.cwd env in
  Eio.Path.save ~create:(`Or_truncate 0o777) (cwd / path) content

let file_contents (filename: string): string =
   let ( / ) = Eio.Path.( / ) in
   Eio_main.run @@ fun env ->
     let cwd = Eio.Stdenv.cwd env in  (* Get the current working directory *)
     let contents = Eio.Path.load (cwd / filename) in
     contents

let uppercase_file (filename: string): unit =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run @@ fun env ->
    let cwd = Eio.Stdenv.cwd env in
    let contents = Eio.Path.load (cwd / filename) in
    let uppercase = String.uppercase_ascii contents in
    Eio.Path.save ~create:(`Or_truncate 0o777) (cwd / filename) uppercase

let count_lines (filename: string): int =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run @@ fun env ->
    let cwd = Eio.Stdenv.cwd env in
    let contents = Eio.Path.load (cwd / filename) in
    let lines = String.split_on_char '\n' contents in
    List.length lines