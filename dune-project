(lang dune 3.7)

(name typesense)

(version dev)

(source (github sabine/typesense-ocaml))
(homepage "https://github.com/sabine/typesense-ocaml")

(license MIT)

(authors
"Sabine Schmaltz")

(maintainers
"Sabine Schmaltz")

(generate_opam_files true)

(package
 (name typesense)
 (synopsis "OCaml HTTP client for Typesense")
 (description "Client bindings for interfacing with Typesense,
an open-source, typo-tolerant search engine.
Abstracts over the HTTP requests to the Typesense server API and
marshalls the responses from the Typesense server into
OCaml data structures to provide a type-safe interface
to your Typesense server.")
 (tags ("typesense" "search" "http client" "client bindings"))
 (depends 
 (ocaml (>= 4.08.0))
 dune
 yojson
 ppx_yojson_conv

 ; riot_client
 riot
 blink
 httpaf ; for blink

 ; cohttp_client
 cohttp
 cohttp-eio
 cohttp-lwt-unix

 uri
 ))
 