[@@@warning "-8"]

let ( let* ) = Result.bind

let make ~meth ?(headers = []) ?(params = []) ~host path =
  let headers =
    Http.Header.init () |> fun h -> Http.Header.add_list h headers
  in
  let path =
    Uri.of_string path |> fun path ->
    Uri.add_query_params path params |> Uri.to_string
  in

  let* conn = Blink.connect (Uri.of_string host) |> Result.map_error (fun e -> match e with
    | `Closed -> `Msg "closed on Blink.connect"
    | (`Invalid_uri u) -> `Msg ("invalid uri: " ^ Uri.to_string u)
    | (`Msg m) -> (`Msg m)
    | `Tls_error _ -> `Msg ("Tls_error")
    | `Unix_error e -> `Msg (Unix.error_message e)
    )
  in
  
  let req = Http.Request.make ~meth ~headers path in
  let* conn = Blink.request conn req () |> Result.map_error (fun e -> match e with
  | `Closed -> `Msg "closed on Blink.request"
  | `Unix_error e -> `Msg (Unix.error_message e)
  ) in
  let* _conn, [ `Status status; `Headers _headers; `Data body; `Done ] =
    Blink.stream conn |> Result.map_error (fun e -> match e with
    | `Closed -> `Msg "closed on Blink.stream"
    | `Eof -> `Msg "Eof"
    | `Response_parsing_error -> `Msg "Response_parsing_error"  
    | `Unix_error e -> `Msg (Unix.error_message e)
    )
  in

  match status with
  | `OK -> Ok (Riot.IO.Buffer.to_string body)
  | _ ->
      Riot.Logger.error (fun f ->
          f "> Got response:\n%s\n%s\n%d\n%!"
            (Http.Status.to_string status)
            (Http.Header.to_lines headers |> String.concat "")
            (Riot.IO.Buffer.length body));

      Error (`Msg "failed to connect")

let get ?(headers = []) ?(params = []) ~host path =
  make ~meth:`GET ~headers ~params ~host path

let post ?(headers = []) ?(params = []) ~host path =
  make ~meth:`POST ~headers ~params ~host path

let delete ?(headers = []) ?(params = []) ~host path =
  make ~meth:`DELETE ~headers ~params ~host path

let patch ?(headers = []) ?(params = []) ~host path =
  make ~meth:`PATCH ~headers ~params ~host path
