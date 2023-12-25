module Make (Config : sig
  val api_key : string
  val url : string
end) =
struct
  module Schema = Typesense.Schema
  module Config = Config

  let headers =
    [
      ("X-TYPESENSE-API-KEY", Config.api_key);
      ("Content-Type", "application/json");
    ]

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

  (* TODO: model and enforce all the response types for these endpoints *)

  module Collection = struct
    let list () = get ~headers "/collections"

    let create schema =
      let body =
        Typesense.Schema.yojson_of_create_schema schema |> Yojson.Safe.to_string
      in
      post ~headers ~body "/collections"

    let delete name =
      let path = "/collections/" ^ Uri.pct_encode name in
      delete ~headers path

    let update name (update_schema : Typesense.Schema.update_schema) =
      let path = "/collections/" ^ Uri.pct_encode name in
      let body =
        Typesense.Schema.yojson_of_update_schema update_schema
        |> Yojson.Safe.to_string
      in
      patch ~headers ~body path
  end

  module Document = struct
    (* TODO: enforce document type to match schema of the collection *)
    let add collection_name document =
      let body = document in
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/documents"
      in
      post ~headers ~body path

    let _import collection_name documents action =
      let body = String.concat "\n" documents in
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/documents/import?action="
        ^ Uri.pct_encode ~component:`Query_value action
      in
      post ~headers ~body path

    let import_create collection_name documents =
      _import collection_name documents "create"

    let import_upsert collection_name documents =
      _import collection_name documents "upsert"

    let import_update collection_name documents =
      _import collection_name documents "update"

    let import_emplace collection_name documents =
      _import collection_name documents "emplace"
  end
end
