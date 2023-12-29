# typesense-ocaml
OCaml HTTP client for Typesense

WARNING: this is not complete, and likely very buggy right now!

Client bindings for interfacing with Typesense,
an open-source, typo-tolerant search engine.
Abstracts over the HTTP requests to the Typesense server API and
marshalls the responses from the Typesense server into
OCaml data structures to provide a type-safe interface
to your Typesense server.

See [typesense.org](https://typesense.org) for more information about Typesense.

Developed against Typesense 0.25.1

## openapi.yml

There's an OpenAPI spec in the Typesense's Go client bindings:

https://raw.githubusercontent.com/typesense/typesense-go/master/typesense/api/generator/openapi.yml

We're not using this at the moment, but it could be helpful to ensure we stay up to date with the Typesense API.