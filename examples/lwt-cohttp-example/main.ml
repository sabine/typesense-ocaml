module Lwt_unix_cohttp = struct
  open Cohttp
  open Cohttp_lwt_unix
  open Lwt.Infix

  let make ~meth ~headers ~params ~body ~host path =
    let uri =
      Uri.of_string (host ^ path) |> fun uri -> Uri.add_query_params uri params
    in
    let headers = Header.of_list headers in

    let body = match body with None -> `Empty | Some b -> `String b in

    Client.call meth ~headers ~body uri >>= fun (resp, body) ->
    let status = Response.status resp in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    match Code.code_of_status status with
    | 200 | 201 -> Ok (`Success body)
    | _ ->
        Error
          (`Msg
            (Printf.sprintf "HTTP error %d\nbody: %s"
               (Code.code_of_status status)
               body))

  let get ~headers ~params ~host path =
    make ~meth:`GET ~headers ~params ~body:None ~host path

  let post ~headers ~params ~host ~body path =
    make ~meth:`POST ~headers ~params ~body:(Some body) ~host path

  let delete ~headers ~params ~host path =
    make ~meth:`DELETE ~headers ~params ~body:None ~host path

  let patch ~headers ~params ~host ~body path =
    make ~meth:`PATCH ~headers ~params ~body:(Some body) ~host path

  let put ~headers ~params ~host ~body path =
    make ~meth:`PUT ~headers ~params ~body:(Some body) ~host path
end

let config =
  let api_key =
    try Sys.getenv "TYPESENSE_API_KEY" with _ -> "{TYPESENSE_API_KEY}"
  in
  let url =
    try Sys.getenv "TYPESENSE_HOST" with _ -> "http://localhost:8108"
  in
  Typesense.Api.{ api_key; url }

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

let make_cohttp_lwt_request = function
  | Typesense.Api.RequestDescriptor.Get { host; path; headers; params } ->
      Lwt_unix_cohttp.get ~headers ~params ~host path
  | Post { host; path; headers; params; body } ->
      Lwt_unix_cohttp.post ~headers ~params ~host ~body path
  | Delete { host; path; headers; params } ->
      Lwt_unix_cohttp.delete ~headers ~params ~host path
  | Patch { host; path; headers; params; body } ->
      Lwt_unix_cohttp.patch ~headers ~params ~host ~body path
  | Put { host; path; headers; params; body } ->
      Lwt_unix_cohttp.put ~headers ~params ~host ~body path

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
  let run_request = print_lwt_req ~make_request:make_cohttp_lwt_request in

  let* () =
    run_request "run_cohttp_lwt_client_tests: create collection"
      (Typesense.Api.Collection.create ~config example_schema)
  in

  let* () =
    run_request "run_cohttp_lwt_client_tests: list collections"
      (Typesense.Api.Collection.list ~config)
  in

  let* () =
    run_request "run_cohttp_lwt_client_tests: update collection"
      (Typesense.Api.Collection.update ~config example_schema.name
         Typesense.Api.Schema.(
           update_schema
             [
               Drop "company_category";
               Add (create_field "company_category" StringArray ~facet:true);
             ]))
  in

  let* () =
    run_request "run_cohttp_lwt_client_tests: delete collection"
      (Typesense.Api.Collection.delete ~config example_schema.name)
  in
  Lwt.return ()

let () = Lwt_main.run (run_cohttp_lwt_client_tests ())
