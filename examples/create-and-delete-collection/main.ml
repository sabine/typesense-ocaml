[@@@warning "-27"]

module Config = struct
  let api_key =
    try Sys.getenv "TYPESENSE_API_KEY" with _ -> "{TYPESENSE_API_KEY}"

  let url = try Sys.getenv "TYPESENSE_HOST" with _ -> "http://localhost:8108"
end

module Typesense = Typesense_api.Make (Config)

let make_request = function
  | Typesense.RequestDescriptor.Get { host; path; headers; params } ->
      Blink_client.get ~headers ~params ~host path
  | Post { host; path; headers; params; body } ->
      Blink_client.post ~headers ~params ~host ~body path
  | Delete { host; path; headers; params } ->
      Blink_client.delete ~headers ~params ~host path
  | Patch { host; path; headers; params; body } ->
      Blink_client.patch ~headers ~params ~host ~body path

let print_req title r =
  print_endline title;
  print_endline @@ Typesense.RequestDescriptor.show_request r;
  print_endline "";
  let response = make_request r in
  match response with
  | Ok response -> print_endline response
  | Error (`Msg m) -> print_endline m

let example_schema =
  Typesense.Schema.(
    schema "companies"
      [
        create_field "company_name" String;
        create_field "num_employees" Int32;
        create_field "country" String ~facet:true;
      ]
      ~default_sorting_field:"num_employees")

let ( let* ) = Result.bind

let () =
  Riot.run @@ fun () ->
  Result.get_ok
  @@
  let* _ = Riot.Logger.start () in

  print_req "create collection" (Typesense.Collection.create example_schema);

  print_req "list collections" (Typesense.Collection.list ());

  print_req "update collection"
    (Typesense.Collection.update example_schema.name
       Typesense.Schema.(
         update_schema
           [
             Drop "company_category";
             Add (create_field "company_category" StringArray ~facet:true);
           ]));

  print_req "delete collection"
    (Typesense.Collection.delete example_schema.name);
  Ok ()
