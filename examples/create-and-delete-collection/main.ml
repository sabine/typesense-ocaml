[@@@warning "-27"]

let config =
  let api_key =
    try Sys.getenv "TYPESENSE_API_KEY" with _ -> "{TYPESENSE_API_KEY}"
  in
  let url =
    try Sys.getenv "TYPESENSE_HOST" with _ -> "http://localhost:8108"
  in
  Typesense.Api.{ api_key; url }

let make_blink_request = function
  | Typesense.Api.RequestDescriptor.Get { host; path; headers; params } ->
      Typesense_blink_riot.get ~headers ~params ~host path
  | Post { host; path; headers; params; body } ->
      Typesense_blink_riot.post ~headers ~params ~host ~body path
  | Delete { host; path; headers; params } ->
      Typesense_blink_riot.delete ~headers ~params ~host path
  | Patch { host; path; headers; params; body } ->
      Typesense_blink_riot.patch ~headers ~params ~host ~body path
  | Put { host; path; headers; params; body } ->
      Typesense_blink_riot.put ~headers ~params ~host ~body path

let print_req ~make_request title r =
  print_endline title;
  print_endline @@ Typesense.Api.RequestDescriptor.show_request r;
  print_endline "";
  let response = make_request r in
  match response with
  | Ok (`Success response) -> print_endline response
  | Error (`Msg m) -> print_endline m

let example_schema =
  Typesense.Api.Schema.(
    schema "companies"
      [
        create_field "company_name" String;
        create_field "num_employees" Int32;
        create_field "country" String ~facet:true;
        create_field "company_category" String ~facet:true;
      ]
      ~default_sorting_field:"num_employees")

let _run_blink_client_tests () =
  let ( let* ) = Result.bind in
  let print_req = print_req ~make_request:make_blink_request in

  Riot.run @@ fun () ->
  Result.get_ok
  @@
  let* _ = Riot.Logger.start () in

  print_req "create collection"
    (Typesense.Api.Collection.create ~config example_schema);

  print_req "list collections" (Typesense.Api.Collection.list ~config);

  print_req "update collection"
    (Typesense.Api.Collection.update ~config example_schema.name
       Typesense.Api.Schema.(
         update_schema
           [
             Drop "company_category";
             Add (create_field "company_category" StringArray ~facet:true);
           ]));

  print_req "delete collection"
    (Typesense.Api.Collection.delete ~config example_schema.name);

  Riot.shutdown () |> ignore;
  Ok ()

let make_cohttp_lwt_request = function
  | Typesense.Api.RequestDescriptor.Get { host; path; headers; params } ->
      Typesense_cohttp_lwt_unix.get ~headers ~params ~host path
  | Post { host; path; headers; params; body } ->
      Typesense_cohttp_lwt_unix.post ~headers ~params ~host ~body path
  | Delete { host; path; headers; params } ->
      Typesense_cohttp_lwt_unix.delete ~headers ~params ~host path
  | Patch { host; path; headers; params; body } ->
      Typesense_cohttp_lwt_unix.patch ~headers ~params ~host ~body path
  | Put { host; path; headers; params; body } ->
      Typesense_cohttp_lwt_unix.put ~headers ~params ~host ~body path

let run_cohttp_lwt_client_tests () =
  let open Lwt.Syntax in
  let print_lwt_req ~make_request title r =
    let* () = Lwt_io.printl title in
    let* () = Lwt_io.printl @@ Typesense.Api.RequestDescriptor.show_request r in
    let* () = Lwt_io.printl "" in
    let* response = make_request r in
    match response with
    | Ok (`Success response) -> Lwt.return (print_endline response)
    | Error (`Msg m) -> Lwt.return (print_endline m)
  in
  let print_req = print_lwt_req ~make_request:make_cohttp_lwt_request in

  let* () =
    print_req "run_cohttp_lwt_client_tests: create collection"
      (Typesense.Api.Collection.create ~config example_schema)
  in

  let* () =
    print_req "run_cohttp_lwt_client_tests: list collections"
      (Typesense.Api.Collection.list ~config)
  in

  let* () =
    print_req "run_cohttp_lwt_client_tests: update collection"
      (Typesense.Api.Collection.update ~config example_schema.name
         Typesense.Api.Schema.(
           update_schema
             [
               Drop "company_category";
               Add (create_field "company_category" StringArray ~facet:true);
             ]))
  in

  let* () =
    print_req "run_cohttp_lwt_client_tests: delete collection"
      (Typesense.Api.Collection.delete ~config example_schema.name)
  in
  Lwt.return ()

let () = Lwt_main.run (run_cohttp_lwt_client_tests ())
(*run_blink_client_tests ()*)
