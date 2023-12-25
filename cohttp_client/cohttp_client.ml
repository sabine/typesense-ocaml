let read_all_to_string (buf_read : Eio.Buf_read.t) :
    (string, [> `Msg of string ]) result =
  Eio.Buf_read.parse ~initial_size:1 ~max_size:1024 Eio.Buf_read.take_all
    (Eio.Buf_read.as_flow buf_read)

let get ?(headers = []) ?(params = []) ~env ~host path =
  let headers =
    Http.Header.init () |> fun h -> Http.Header.add_list h headers
  in
  let path =
    Uri.of_string path |> fun path ->
    Uri.add_query_params path params |> Uri.to_string
  in
  let response, body = Cohttp_eio.Client.get ~headers ~host env path in
  match response.status with
  | `OK ->
      let body = read_all_to_string body in
      Ok body
  | _ -> failwith "unimplemented"
