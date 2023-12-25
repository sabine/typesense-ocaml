module Config = struct
  let api_key =
    try Sys.getenv "TYPESENSE_API_KEY" with _ -> "{TYPESENSE_API_KEY}"

  let url = try Sys.getenv "TYPESENSE_HOST" with _ -> "{TYPESENSE_HOST}"
end

module Typesense = Typesense_api.Make (Config)

let print_req title r =
  print_endline title;
  print_endline @@ Typesense.show_request r;
  print_endline ""

let example_schema =
  Typesense.Schema.schema "companies"
    Typesense.Schema.[ create_field "company_name" String ]

let () =
  print_req "create collection" (Typesense.Collection.create example_schema);

  print_req "list collections" (Typesense.Collection.list ());

  print_req "delete collection"
    (Typesense.Collection.delete example_schema.name);

  print_req "update collection"
    (Typesense.Collection.update example_schema.name
       Typesense.Schema.
         {
           fields =
             [
               Drop "company_category";
               Add (create_field "company_category" StringArray ~facet:true);
             ];
         })
