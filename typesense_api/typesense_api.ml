module Make (Config : sig
  val api_key : string
  val url : string
end) =
struct
  module Schema = Typesense.Schema
  module Config = Config

  let headers = [ ("X-TYPESENSE-API-KEY", Config.api_key) ]

  type request =
    | Get of {
        host : string;
        path : string;
        headers : (string * string) list;
        params : (string * string list) list;
      }
    | Post of {
        host : string;
        path : string;
        headers : (string * string) list;
        params : (string * string list) list;
        body : string;
      }
    | Delete of {
        host : string;
        path : string;
        headers : (string * string) list;
      }
    | Patch of {
        host : string;
        path : string;
        headers : (string * string) list;
        body : string;
      }
  [@@deriving show]

  let get ?(headers = []) ?(params = []) path =
    Get { host = Config.url; path; headers; params }

  let post ?(headers = []) ?(params = []) ~body path =
    Post { host = Config.url; path; headers; params; body }

  let delete ~headers path = Delete { host = Config.url; path; headers }

  let patch ~headers ~body path =
    Patch { host = Config.url; path; headers; body }

  (* collections *)

  let collections () = get ~headers "/collections"

  let create_collection schema =
    let body =
      Typesense.Schema.yojson_of_create_schema schema |> Yojson.Safe.to_string
    in
    post ~headers ~body "/collections"

  let delete_collection name =
    let path = "/collections/" ^ Uri.pct_encode name in
    delete ~headers path

  let update_collection name (update_schema : Typesense.Schema.update_schema) =
    let path = "/collections/" ^ Uri.pct_encode name in
    let body =
      Typesense.Schema.yojson_of_update_schema update_schema
      |> Yojson.Safe.to_string
    in
    patch ~headers ~body path
end
