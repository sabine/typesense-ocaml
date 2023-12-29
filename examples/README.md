To run the examples, you need to provide TYPESENSE_HOST and TYPESENSE_API_KEY environment variables, e.g.:

```
TYPESENSE_API_KEY={YOUR_TYPESENSE_API_KEY} TYPESENSE_HOST=http://localhost:8108 opam exec -- dune exec examples/create-and-delete-collection/main.exe
```

Typesense can be installed locally:

https://typesense.org/docs/guide/install-typesense.html#option-2-local-machine-self-hosting

The Typesense installation page also lists the location where you can find the necessary TYPESENSE_API_KEY.