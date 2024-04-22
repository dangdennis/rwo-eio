(lang dune 3.15)

(name rwo_eio)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name rwo_eio)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (>= 5.1.1))
  (dune
   (>= 3.15))
  (eio_main
   (= 1.0))
  (uri
   (= 4.4.0))
  (yojson
   (= 2.1.2))
  (cohttp-eio
   (= 6.0.0~beta2))
  (textwrap
   (= 0.2.1)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
