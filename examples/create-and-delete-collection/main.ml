module Config = struct
  let api_key = "abc"
  let url = ""
end

module Typesense = Typesense_api.Make (Config)

let print_req title r =
  print_endline title;
  print_endline @@ Typesense.show_request r;
  print_endline ""

let example_schema =
  Typesense.Schema.schema "companies"
    Typesense.Schema.[ field "company_name" String ]

let () =
  print_req "list collections" (Typesense.collections ());

  print_req "list collections" (Typesense.create_collection example_schema)
