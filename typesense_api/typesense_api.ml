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
  [@@deriving show]

  let get ?(headers = []) ?(params = []) path =
    Get { host = Config.url; path; headers; params }

  let post ?(headers = []) ?(params = []) ~body path =
    Post { host = Config.url; path; headers; params; body }

  let collections () = get ~headers "/collections"

  let create_collection schema =
    post ~headers
      ~body:(Typesense.Schema.yojson_of_t schema |> Yojson.Safe.to_string)
      "/collections"
end
