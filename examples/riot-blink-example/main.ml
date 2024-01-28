module Typesense_blink_riot = struct
  [@@@warning "-8"]

  let ( let* ) = Result.bind

  let make ~meth ?(headers = []) ?(params = []) ?(body = None) ~host path =
    let headers =
      Http.Header.init () |> fun h -> Http.Header.add_list h headers
      (*|> (fun h -> Http.Header.add h "connection" "keep-alive")*)
    in
    let path =
      Uri.of_string path |> fun path ->
      Uri.add_query_params path params |> Uri.to_string
    in

    let* conn =
      Blink.connect (Uri.of_string host)
      |> Result.map_error (fun e ->
             match e with
             | `Closed -> `Msg "closed on Blink.connect"
             | `Invalid_uri u -> `Msg ("invalid uri: " ^ Uri.to_string u)
             | `Msg m -> `Msg m
             | `Tls_error _ -> `Msg "Tls_error"
             | `Unix_error e -> `Msg (Unix.error_message e))
    in

    let req = Http.Request.make ~meth ~headers path in
    let body = body |> Option.map Riot.IO.Buffer.of_string in
    let* conn =
      Blink.request ?body conn req ()
      |> Result.map_error (fun e ->
             match e with
             | `Closed -> `Msg "closed on Blink.request"
             | `Unix_error e -> `Msg (Unix.error_message e))
    in
    let* _conn, [ `Status status; `Headers _headers; `Data body; `Done ] =
      Blink.stream conn
      |> Result.map_error (fun e ->
             match e with
             | `Closed -> `Msg "closed on Blink.stream"
             | `Eof -> `Msg "Eof"
             | `Response_parsing_error -> `Msg "Response_parsing_error"
             | `Unix_error e -> `Msg (Unix.error_message e))
    in

    match status with
    | `OK -> Ok (`Success (Riot.IO.Buffer.to_string body))
    | _ ->
        Riot.Logger.error (fun f ->
            f "> Got response:\n%s\n%s\n%d\n%!"
              (Http.Status.to_string status)
              (Http.Header.to_lines headers |> String.concat "")
              (Riot.IO.Buffer.length body));

        Error
          (`Msg
            (Printf.sprintf "> Got response:\n%s\n%s\n%d\n%!"
               (Http.Status.to_string status)
               (Http.Header.to_lines headers |> String.concat "")
               (Riot.IO.Buffer.length body)))

  let get ?(headers = []) ?(params = []) ~host path =
    make ~meth:`GET ~headers ~params ~host path

  let post ?(headers = []) ?(params = []) ~host ~body path =
    make ~meth:`POST ~headers ~params ~host ~body:(Some body) path

  let delete ?(headers = []) ?(params = []) ~host path =
    make ~meth:`DELETE ~headers ~params ~host path

  let patch ?(headers = []) ?(params = []) ~host ~body path =
    make ~meth:`PATCH ~headers ~params ~host ~body:(Some body) path

  let put ?(headers = []) ?(params = []) ~host ~body path =
    make ~meth:`PUT ~headers ~params ~host ~body:(Some body) path
end

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

let run_request ~make_request title r =
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

let run_blink_client_tests () =
  let ( let* ) = Result.bind in
  let run_request = run_request ~make_request:make_blink_request in

  Riot.run @@ fun () ->
  Result.get_ok
  @@
  let* _ = Riot.Logger.start () in

  run_request "create collection"
    (Typesense.Api.Collection.create ~config example_schema);

  run_request "list collections" (Typesense.Api.Collection.list ~config);

  run_request "update collection"
    (Typesense.Api.Collection.update ~config example_schema.name
       Typesense.Api.Schema.(
         update_schema
           [
             Drop "company_category";
             Add (create_field "company_category" StringArray ~facet:true);
           ]));

  run_request "delete collection"
    (Typesense.Api.Collection.delete ~config example_schema.name);

  Riot.shutdown () |> ignore;
  Ok ()

let () = run_blink_client_tests ()
