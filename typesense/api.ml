type config = { api_key : string; url : string }

open Ppx_yojson_conv_lib.Yojson_conv

module Yojson = struct
  include Yojson

  module Safe = struct
    include Yojson.Safe

    let t_of_yojson v = v
  end
end

module Params = struct
  let add_if_string name value =
    if String.length value > 0 then [ (name, [ value ]) ] else []

  let add_if_bool name value =
    match value with
    | None -> []
    | Some true -> [ (name, [ "true" ]) ]
    | Some false -> [ (name, [ "false" ]) ]

  let add_if_int name value =
    match value with None -> [] | Some i -> [ (name, [ string_of_int i ]) ]
end

module RequestDescriptor = struct
  let headers ~config =
    [
      ("X-TYPESENSE-API-KEY", config.api_key);
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
        params : (string * string list) list;
      }
    | Patch of {
        host : string;
        path : string;
        headers : (string * string) list;
        params : (string * string list) list;
        body : string;
      }
    | Put of {
        host : string;
        path : string;
        headers : (string * string) list;
        params : (string * string list) list;
        body : string;
      }
  [@@deriving show]

  let get ~config ?(headers = headers ~config) ?(params = []) path =
    Get { host = config.url; path; headers; params }

  let post ~config ?(headers = headers ~config) ?(params = []) ~body path =
    Post { host = config.url; path; headers; params; body }

  let delete ~config ?(headers = headers ~config) ?(params = []) path =
    Delete { host = config.url; path; headers; params }

  let patch ~config ?(headers = headers ~config) ?(params = []) ~body path =
    Patch { host = config.url; path; headers; params; body }

  let put ~config ?(headers = headers ~config) ?(params = []) ~body path =
    Put { host = config.url; path; headers; params; body }
end
(* TODO: model and enforce all the response types for these endpoints *)

module Collection = struct
  let create ~config schema =
    let body = Schema.yojson_of_create_schema schema |> Yojson.Safe.to_string in
    RequestDescriptor.post ~config ~body "/collections"

  let clone ~config existing_collection_name new_collection_name =
    let path = "/collections" in
    let params = [ ("src_name", [ existing_collection_name ]) ] in
    let body =
      Yojson.Safe.to_string (`Assoc [ ("name", `String new_collection_name) ])
    in
    RequestDescriptor.post ~config ~params ~body path

  let get ~config collection_name =
    let path = "/collections/" ^ Uri.pct_encode collection_name in
    RequestDescriptor.get ~config path

  let list ~config = RequestDescriptor.get ~config "/collections"

  let delete ~config collection_name =
    let path = "/collections/" ^ Uri.pct_encode collection_name in
    RequestDescriptor.delete ~config path

  let update ~config collection_name (update_schema : Schema.update_schema) =
    let path = "/collections/" ^ Uri.pct_encode collection_name in
    let body =
      Schema.yojson_of_update_schema update_schema |> Yojson.Safe.to_string
    in
    RequestDescriptor.patch ~config ~body path

  module Alias = struct
    let create_or_update ~config collection_name alias =
      let path = "/aliases/" ^ Uri.pct_encode collection_name in
      let body =
        Yojson.Safe.to_string (`Assoc [ ("collection_name", `String alias) ])
      in
      RequestDescriptor.put ~config ~body path

    let get ~config alias =
      let path = "/aliases/" ^ Uri.pct_encode alias in
      RequestDescriptor.get ~config path

    let list ~config =
      let path = "/aliases" in
      RequestDescriptor.get ~config path

    let delete ~config alias =
      let path = "/aliases/" ^ Uri.pct_encode alias in
      RequestDescriptor.delete ~config path
  end
end

module Document = struct
  (* TODO: enforce document type to match schema of the collection *)

  type dirty_values = Coerce_or_reject | Coerce_or_drop | Drop | Reject

  let string_of_dirty_values v =
    match v with
    | Some Coerce_or_reject -> "coerce_or_reject"
    | Some Coerce_or_drop -> "coerce_or_drop"
    | Some Drop -> "drop"
    | Some Reject -> "reject"
    | None -> ""

  type document_write_action = Create | Upsert | Update | Emplace

  let string_of_document_write_action = function
    | Create -> "create"
    | Upsert -> "upsert"
    | Update -> "update"
    | Emplace -> "emplace"

  let add ~config ?(dirty_values = None) ?remote_embedding_timeout_ms
      ?remote_embedding_num_tries ?(action = Create) ~collection_name document =
    let body = document |> Yojson.Safe.to_string in
    let path =
      "/collections/" ^ Uri.pct_encode collection_name ^ "/documents"
    in
    let params =
      let open Params in
      add_if_string "dirty_values" (string_of_dirty_values dirty_values)
      @ add_if_int "remote_embedding_timeout_ms" remote_embedding_timeout_ms
      @ add_if_int "remote_embedding_num_tries" remote_embedding_num_tries
      @ add_if_string "action" (string_of_document_write_action action)
    in
    RequestDescriptor.post ~config ~params ~body path

  module ImportResponse = struct
    type t = {
      success : bool;
      code : int32 option;
      error : string option;
      document : string; (* deserialize to JSON value *)
    }
    [@@deriving of_yojson] [@@yojson.allow_extra_fields]

    let t_of_string s =
      String.split_on_char '\n' s
      |> List.map (fun s -> Yojson.Safe.from_string s |> t_of_yojson)
  end

  let import ~config ?(dirty_values = None) ?(batch_size = None)
      ?remote_embedding_timeout_ms ?remote_embedding_num_tries
      ?(action = Create) ~collection_name documents =
    let body = String.concat "\n" (List.map Yojson.Safe.to_string documents) in
    let params =
      let open Params in
      [ ("action", [ string_of_document_write_action action ]) ]
      @ add_if_string "dirty_values" (string_of_dirty_values dirty_values)
      @ add_if_int "batch_size" batch_size
      @ add_if_int "remote_embedding_timeout_ms" remote_embedding_timeout_ms
      @ add_if_int "remote_embedding_num_tries" remote_embedding_num_tries
    in
    let path =
      "/collections/" ^ Uri.pct_encode collection_name ^ "/documents/import"
    in
    RequestDescriptor.post ~config ~params ~body path

  let get ~config ~collection_name ~document_id =
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/documents/" ^ document_id
    in
    RequestDescriptor.get ~config path

  let update ~config ?(dirty_values = None) ~collection_name ~document_id
      document_patch =
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/documents/" ^ document_id
    in
    let params =
      Params.add_if_string "dirty_values" (string_of_dirty_values dirty_values)
    in
    let body = document_patch |> Yojson.Safe.to_string in
    RequestDescriptor.patch ~config ~params ~body path

  let update_by_query ~config ~filter_by ~collection_name document_patch =
    let path =
      "/collections/" ^ Uri.pct_encode collection_name ^ "/documents"
    in
    let params = [ ("filter_by", [ filter_by ]) ] in
    let body = document_patch |> Yojson.Safe.to_string in
    RequestDescriptor.patch ~config ~params ~body path

  let delete ~config ~collection_name ~document_id =
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/documents/" ^ document_id
    in
    RequestDescriptor.delete ~config path

  let delete_by_query ~config ~filter_by ~collection_name =
    let path =
      "/collections/" ^ Uri.pct_encode collection_name ^ "/documents"
    in
    let params = [ ("filter_by", [ filter_by ]) ] in
    RequestDescriptor.delete ~config ~params path

  let export ~config ?(filter_by = "") ?(include_fields = "")
      ?(exclude_fields = "") ~collection_name () =
    let path =
      "/collections/" ^ Uri.pct_encode collection_name ^ "/documents/export"
    in
    let params =
      (if String.length filter_by > 0 then [ ("filter_by", [ filter_by ]) ]
       else [])
      @ (if String.length include_fields > 0 then
           [ ("include_fields", [ include_fields ]) ]
         else [])
      @
      if String.length exclude_fields > 0 then
        [ ("exclude_fields", [ exclude_fields ]) ]
      else []
    in
    RequestDescriptor.get ~config ~params path
end

module Search = struct
  [@@@ocamlformat "disable"]

  let make_search_params
    (* query parameters*)
    ~q
    ~query_by
    ?(prefix="")
    ?(infix="")
    ?pre_segmented_query
    ?(preset="")
    (* filter parameters *)
    ?(filter_by = "")
    (* ranking and sorting parameters*)
    ?(query_by_weights="")
    ?(text_match_type="")
    ?(sort_by="")
    ?prioritize_exact_match
    ?prioritize_token_position
    ?(pinned_hits="")
    ?(hidden_hits="")
    ?enable_overrides
    (* pagination parameters *)
    ?page
    ?per_page
    ?offset
    ?limit
    (* faceting parameters *)
    ?(facet_by="")
    ?max_facet_values
    ?(facet_query="")
    ?facet_query_num_typos
    (* grouping parameters *)
    ?(group_by="")
    ?group_limit
    (* results parameters *)
    ?(include_fields="")
    ?(exclude_fields="")
    ?(highlight_fields="")
    ?(highlight_full_fields="")
    ?highlight_affix_num_tokens
    ?(highlight_start_tag="")
    ?(highlight_end_tag="")
    ?enable_highlight_v1
    ?snippet_threshold
    ?limit_hits
    ?search_cutoff_ms
    ?max_candidates
    ?exhaustive_search
    (* typo-tolerance parameters *)
    ?num_typos
    ?min_len_1typo
    ?min_len_2typo
    ?(split_join_tokens="")
    ?typo_tokens_threshold
    ?drop_tokens_threshold
    (* caching parameters *)
    ?use_cache
    ?cache_ttl
    (* vector queries *)
    ?(vector_query="")
    ?remote_embedding_timeout_ms
    ?remote_embedding_num_tries
    (* multi-search parameters *)
    ?limit_multi_searches
    () =
    let open Params in
    [("q", [q]); ("query_by", [query_by])]
    @ add_if_string "prefix" prefix
    @ add_if_string "infix" infix
    @ add_if_bool "pre_segmented_query" pre_segmented_query
    @ add_if_string "preset" preset
    (* filter parameters *)
    @ add_if_string "filter_by" filter_by
    (* ranking and sorting parameters*)
    @ add_if_string "query_by_weights" query_by_weights
    @ add_if_string "text_match_type" text_match_type
    @ add_if_string "sort_by" sort_by
    @ add_if_bool "prioritize_exact_match" prioritize_exact_match
    @ add_if_bool "prioritize_token_position" prioritize_token_position
    @ add_if_string "pinned_hits" pinned_hits
    @ add_if_string "hidden_hits" hidden_hits
    @ add_if_bool "enable_overrides" enable_overrides
    (* pagination parameters *)
    @ add_if_int "page" page
    @ add_if_int "per_page" per_page
    @ add_if_int "offset" offset
    @ add_if_int "limit" limit
    (* faceting parameters *)
    @ add_if_string "facet_by" facet_by
    @ add_if_int "max_facet_values" max_facet_values
    @ add_if_string "facet_query" facet_query
    @ add_if_int "facet_query_num_typos" facet_query_num_typos
    (* grouping parameters *)
    @ add_if_string "group_by" group_by
    @ add_if_int "group_limit" group_limit
    (* results parameters *)
    @ add_if_string "include_fields" include_fields
    @ add_if_string "exclude_fields" exclude_fields
    @ add_if_string "highlight_fields" highlight_fields
    @ add_if_string "highlight_full_fields" highlight_full_fields
    @ add_if_int "highlight_affix_num_tokens" highlight_affix_num_tokens
    @ add_if_string "highlight_start_tag" highlight_start_tag
    @ add_if_string "highlight_end_tag" highlight_end_tag
    @ add_if_bool "enable_highlight_v1" enable_highlight_v1
    @ add_if_int "snippet_threshold" snippet_threshold
    @ add_if_int "limit_hits" limit_hits
    @ add_if_int "search_cutoff_ms" search_cutoff_ms
    @ add_if_int "max_candidates" max_candidates
    @ add_if_bool "exhaustive_search" exhaustive_search
    (* typo-tolerance parameters *)
    @ add_if_int "num_typos" num_typos
    @ add_if_int "min_len_1typo" min_len_1typo
    @ add_if_int "min_len_2typo" min_len_2typo
    @ add_if_string "split_join_tokens" split_join_tokens
    @ add_if_int "typo_tokens_threshold" typo_tokens_threshold
    @ add_if_int "drop_tokens_threshold" drop_tokens_threshold
    (* caching parameters *)
    @ add_if_bool "use_cache" use_cache
    @ add_if_int "cache_ttl" cache_ttl
    (* vector queries*)
    @ add_if_string "vector_query" vector_query
    @ add_if_int "remote_embedding_timeout_ms" remote_embedding_timeout_ms
    @ add_if_int "remote_embedding_num_tries" remote_embedding_num_tries
    (* multis-earch params *)
    @ add_if_int "limit_multi_searches" limit_multi_searches

  module FacetCounts = struct
    type facet_count = { count : int; value : string } [@@deriving of_yojson] [@@yojson.allow_extra_fields]
    type stats = { max : float; min : float; sum : float; total_values: int; avg: float } [@@deriving of_yojson] [@@yojson.allow_extra_fields]
    type facet_counts = {
      counts: facet_count list;
      field_name: string;
      stats: stats;
    } [@@deriving of_yojson] [@@yojson.allow_extra_fields]
    type t = (string * facet_counts) list

    let t_of_yojson v =
      let decode_kv_pair (k,v) =
        (k, facet_counts_of_yojson v)
      in
      match v with
    | `Assoc l ->
      List.map decode_kv_pair l
    |_ -> raise (Invalid_argument "FacetCounts.y_of_yojson expected an object")
  end

  type highlight = {
    field: string;
    snippet: string;
  } [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  module GeoDistanceMeters = struct
    type t = (string, float) Hashtbl.t

    let t_of_yojson v =
      match v with
      | `Assoc l ->
          let tbl = Hashtbl.create (List.length l) in
          let decode_kv_pair = function
            | k, `Float v -> Hashtbl.add tbl k v
            | _ -> raise (Invalid_argument "expected a float value")
          in
          l |> List.iter decode_kv_pair;
          tbl
      | _ -> raise (Invalid_argument "GeoDistanceMeters.t_of_yojson expected an object")
  end

  type search_result_hit = {
    highlight: Yojson.Safe.t;
    document : Yojson.Safe.t;
    text_match: int64;
    geo_distance_meters: GeoDistanceMeters.t;
    vector_distance: float;
  } [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  type search_grouped_hit = {
    found: int;
    group_key : string list;
    hits: search_result_hit list;
  } [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  type search_response = {
    facet_counts : FacetCounts.t;
    found : int;
    search_time_ms : int;
    out_of : int;
    search_cutoff: bool;
    page : int;
    grouped_hits: search_grouped_hit list;
    hits : search_result_hit list;
    request_params : Yojson.Safe.t;
  } [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let search ~config ?(x_typesense_user_id = "") ~search_params ~collection_name () =
    let path =
      "/collections/" ^ Uri.pct_encode collection_name ^ "/documents/search"
    in
    let headers =
      if x_typesense_user_id <> "" then
        ("X-TYPESENSE-USER-ID", x_typesense_user_id)
        :: RequestDescriptor.headers ~config
      else RequestDescriptor.headers ~config
    in
    RequestDescriptor.get ~config ~params:search_params ~headers path

  type single_search = {
    collection : string; [@default ""] [@yojson_drop_default ( = )]
    q : string;
    query_by : string;
    prefix : string; [@default ""] [@yojson_drop_default ( = )]
    infix : string; [@default ""] [@yojson_drop_default ( = )]
    pre_segmented_query : bool option;
        [@default None] [@yojson_drop_default ( = )]
    preset : string; [@default ""] [@yojson_drop_default ( = )]
    (* filter parameters *)
    filter_by : string; [@default ""] [@yojson_drop_default ( = )]
    (* ranking and sorting parameters*)
    query_by_weights : string; [@default ""] [@yojson_drop_default ( = )]
    text_match_type : string; [@default ""] [@yojson_drop_default ( = )]
    sort_by : string; [@default ""] [@yojson_drop_default ( = )]
    prioritize_exact_match : bool option;
        [@default None] [@yojson_drop_default ( = )]
    prioritize_token_position : bool option;
        [@default None] [@yojson_drop_default ( = )]
    pinned_hits : string; [@default ""] [@yojson_drop_default ( = )]
    hidden_hits : string; [@default ""] [@yojson_drop_default ( = )]
    enable_overrides : bool option;
        [@default None] [@yojson_drop_default ( = )]
    (* pagination parameters *)
    page : int option; [@default None] [@yojson_drop_default ( = )]
    per_page : int option; [@default None] [@yojson_drop_default ( = )]
    offset : int option; [@default None] [@yojson_drop_default ( = )]
    limit : int option; [@default None] [@yojson_drop_default ( = )]
    (* faceting parameters *)
    facet_by : string; [@default ""] [@yojson_drop_default ( = )]
    max_facet_values : int option;
        [@default None] [@yojson_drop_default ( = )]
    facet_query : string; [@default ""] [@yojson_drop_default ( = )]
    facet_query_num_typos : int option;
        [@default None] [@yojson_drop_default ( = )]
    (* grouping parameters *)
    group_by : string; [@default ""] [@yojson_drop_default ( = )]
    group_limit : int option; [@default None] [@yojson_drop_default ( = )]
    (* results parameters *)
    include_fields : string; [@default ""] [@yojson_drop_default ( = )]
    exclude_fields : string; [@default ""] [@yojson_drop_default ( = )]
    highlight_fields : string; [@default ""] [@yojson_drop_default ( = )]
    highlight_full_fields : string;
        [@default ""] [@yojson_drop_default ( = )]
    highlight_affix_num_tokens : int option;
        [@default None] [@yojson_drop_default ( = )]
    highlight_start_tag : string; [@default ""] [@yojson_drop_default ( = )]
    highlight_end_tag : string; [@default ""] [@yojson_drop_default ( = )]
    enable_highlight_v1 : bool option;
        [@default None] [@yojson_drop_default ( = )]
    snippet_threshold : int option;
        [@default None] [@yojson_drop_default ( = )]
    limit_hits : int option; [@default None] [@yojson_drop_default ( = )]
    search_cutoff_ms : int option;
        [@default None] [@yojson_drop_default ( = )]
    max_candidates : int option;
        [@default None] [@yojson_drop_default ( = )]
    exhaustive_search : bool option;
        [@default None] [@yojson_drop_default ( = )]
    (* typo-tolerance parameters *)
    num_typos : int option; [@default None] [@yojson_drop_default ( = )]
    min_len_1typo : int option; [@default None] [@yojson_drop_default ( = )]
    min_len_2typo : int option; [@default None] [@yojson_drop_default ( = )]
    split_join_tokens : string; [@default ""] [@yojson_drop_default ( = )]
    typo_tokens_threshold : int option;
        [@default None] [@yojson_drop_default ( = )]
    drop_tokens_threshold : int option;
        [@default None] [@yojson_drop_default ( = )]
    (* caching parameters *)
    use_cache : bool option; [@default None] [@yojson_drop_default ( = )]
    cache_ttl : int option; [@default None] [@yojson_drop_default ( = )]
    (* vector queries*)
    vector_query : string; [@default ""] [@yojson_drop_default ( = )]
    remote_embedding_timeout_ms : int option;
        [@default None] [@yojson_drop_default ( = )]
    remote_embedding_num_tries : int option;
        [@default None] [@yojson_drop_default ( = )]
    (* multi search parameters *)
    x_typesense_api_key : string;
        [@key "x-typesense-api-key"]
        [@default ""]
        [@yojson_drop_default ( = )]
  }
  [@@deriving yojson_of]

  [@@@ocamlformat "disable"]

  let make_single_search ~query_by
    ~q
    ?(prefix="")
    ?(infix="")
    ?pre_segmented_query
    ?(preset="")
    (* filter parameters *)
    ?(filter_by = "")
    (* ranking and sorting parameters*)
    ?(query_by_weights="")
    ?(text_match_type="")
    ?(sort_by="")
    ?prioritize_exact_match
    ?prioritize_token_position
    ?(pinned_hits="")
    ?(hidden_hits="")
    ?enable_overrides
    (* pagination parameters *)
    ?page
    ?per_page
    ?offset
    ?limit
    (* faceting parameters *)
    ?(facet_by="")
    ?max_facet_values
    ?(facet_query="")
    ?facet_query_num_typos
    (* grouping parameters *)
    ?(group_by="")
    ?group_limit
    (* results parameters *)
    ?(include_fields="")
    ?(exclude_fields="")
    ?(highlight_fields="")
    ?(highlight_full_fields="")
    ?highlight_affix_num_tokens
    ?(highlight_start_tag="")
    ?(highlight_end_tag="")
    ?enable_highlight_v1
    ?snippet_threshold
    ?limit_hits
    ?search_cutoff_ms
    ?max_candidates
    ?exhaustive_search
    (* typo-tolerance parameters *)
    ?num_typos
    ?min_len_1typo
    ?min_len_2typo
    ?(split_join_tokens="")
    ?typo_tokens_threshold
    ?drop_tokens_threshold
    (* caching parameters *)
    ?use_cache
    ?cache_ttl
    (* vector queries *)
    ?(vector_query="")
    ?remote_embedding_timeout_ms
    ?remote_embedding_num_tries
    (* multi-search parameters *)
    ?(collection="")
    ?(x_typesense_api_key="")
    ()
    =
    {
      collection;
      q;
      query_by;
      prefix;
      infix;
      pre_segmented_query;
      preset;
      filter_by;
      query_by_weights;
      text_match_type;
      sort_by;
      prioritize_exact_match;
      prioritize_token_position;
      pinned_hits;
      hidden_hits;
      enable_overrides;
      page;
      per_page;
      offset;
      limit;
      facet_by;
      max_facet_values;
      facet_query;
      facet_query_num_typos;
      group_by;
      group_limit;
      include_fields;
      exclude_fields;
      highlight_fields;
      highlight_full_fields;
      highlight_affix_num_tokens;
      highlight_start_tag;
      highlight_end_tag;
      enable_highlight_v1;
      snippet_threshold;
      limit_hits;
      search_cutoff_ms;
      max_candidates;
      exhaustive_search;
      num_typos;
      min_len_1typo;
      min_len_2typo;
      split_join_tokens;
      typo_tokens_threshold;
      drop_tokens_threshold;
      use_cache;
      cache_ttl;
      vector_query;
      remote_embedding_timeout_ms;
      remote_embedding_num_tries;
      x_typesense_api_key;
    }

  type multi_search_request = {
      searches : single_search list;
    } [@@deriving yojson_of]

  let perform_multi_search ~config ~search_requests ~common_search_params ?(x_typesense_user_id="") ~collection_name () =
    let body =
      search_requests |> yojson_of_multi_search_request
      |> Yojson.Safe.to_string
    in
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/documents/multi-search"
    in
    let headers =
      if x_typesense_user_id <> "" then
      ("X-TYPESENSE-USER-ID", x_typesense_user_id) :: RequestDescriptor.headers ~config
      else RequestDescriptor.headers ~config
    in
    RequestDescriptor.post ~config ~params:common_search_params ~body ~headers path
end

module Analytics = struct
  let create_rule ~config ~name ~rule_type ~source_collections
      ~destination_collection ~limit =
    let path = "/analytics/rules" in
    let body =
      `Assoc
        [
          ("name", name);
          ("type", rule_type);
          ( "params",
            `Assoc
              [
                ( "source",
                  `Assoc
                    [
                      ( "collections",
                        `List (List.map (fun s -> `String s) source_collections)
                      );
                    ] );
                ( "destination",
                  `Assoc [ ("collection", `String destination_collection) ] );
                ("limit", limit);
              ] );
        ]
      |> Yojson.Safe.to_string
    in
    RequestDescriptor.post ~config ~body path

  let list_rules ~config =
    let path = "/analytics/rules" in
    RequestDescriptor.get ~config path

  let delete_rule ~config ~rule_name =
    let path = "/analytics/rules/" ^ Uri.pct_encode rule_name in
    RequestDescriptor.delete ~config path
end

module Keys = struct
  type create_key = {
    actions : string list;
    collections : string list;
    description : string;
    value : string; [@default ""] [@yojson_drop_default ( = )]
    expires_at : int64; [@default 0L] [@yojson_drop_default ( = )]
  }
  [@@deriving yojson_of]

  type create_key_response = {
    id : int;
    actions : string list;
    collections : string list;
    description : string;
    value : string;
    expires_at : int64;
  }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let create_key ~config ~create_key =
    let path = "/keys" in
    let body = create_key |> yojson_of_create_key |> Yojson.Safe.to_string in
    RequestDescriptor.post ~config ~body path

  type get_key_response = {
    id : int;
    actions : string list;
    collections : string list;
    description : string;
    value_prefix : string;
    expires_at : int64;
  }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let get_key ~config ~key_id =
    let path = "/keys/" ^ Uri.pct_encode (string_of_int key_id) in
    RequestDescriptor.get ~config path

  type list_keys_response = { keys : get_key_response list }

  let list_keys ~config =
    let path = "/keys" in
    RequestDescriptor.get ~config path

  type delete_key_response = { id : int }

  let delete_key ~config ~key_id =
    let path = "/keys/" ^ Uri.pct_encode (string_of_int key_id) in
    RequestDescriptor.delete ~config path
end

module Override = struct
  type override_rule = {
    query : string; [@default ""] [@yojson_drop_default ( = )]
    filter_by : string; [@default ""] [@yojson_drop_default ( = )]
    _match : string; [@key "match"] [@default ""] [@yojson_drop_default ( = )]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  type override_include = { id : string; position : int }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  type override_exclude = { id : string }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  type override = {
    rule : override_rule;
    includes : override_include list option;
        [@default None] [@yojson_drop_default ( = )]
    excludes : override_exclude list option;
        [@default None] [@yojson_drop_default ( = )]
    filter_by : string option; [@default None] [@yojson_drop_default ( = )]
    sort_by : string option; [@default None] [@yojson_drop_default ( = )]
    replace_query : bool option; [@default None] [@yojson_drop_default ( = )]
    remove_matched_tokens : bool option;
        [@default None] [@yojson_drop_default ( = )]
    filter_curated_hits : bool option;
        [@default None] [@yojson_drop_default ( = )]
    effective_from_ts : int64 option;
        [@default None] [@yojson_drop_default ( = )]
    effective_to_ts : int64 option; [@default None] [@yojson_drop_default ( = )]
    stop_processing : bool option; [@default None] [@yojson_drop_default ( = )]
  }
  [@@deriving yojson_of]

  type create_override_response = {
    id : string;
    rule : override_rule;
    includes : override_include list option;
        [@default None] [@yojson_drop_default ( = )]
    excludes : override_exclude list option;
        [@default None] [@yojson_drop_default ( = )]
    filter_by : string option; [@default None] [@yojson_drop_default ( = )]
    sort_by : string option; [@default None] [@yojson_drop_default ( = )]
    replace_query : bool option; [@default None] [@yojson_drop_default ( = )]
    remove_matched_tokens : bool option;
        [@default None] [@yojson_drop_default ( = )]
    filter_curated_hits : bool option;
        [@default None] [@yojson_drop_default ( = )]
    effective_from_ts : int64 option;
        [@default None] [@yojson_drop_default ( = )]
    effective_to_ts : int64 option; [@default None] [@yojson_drop_default ( = )]
    stop_processing : bool option; [@default None] [@yojson_drop_default ( = )]
  }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let create ~config ~collection_name ~override_id ~override =
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/overrides/" ^ Uri.pct_encode override_id
    in
    let body = override |> yojson_of_override |> Yojson.Safe.to_string in
    RequestDescriptor.post ~config ~body path

  type list_override_response = { overrides : create_override_response list }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let list ~config ~collection_name =
    let path =
      "/collections/" ^ Uri.pct_encode collection_name ^ "/overrides"
    in
    RequestDescriptor.get ~config path

  type delete_override_response = { id : string }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let delete ~config ~collection_name ~override_id =
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/overrides/" ^ Uri.pct_encode override_id
    in
    RequestDescriptor.delete ~config path
end

module Synonym = struct
  type synonyms = {
    synonyms : string list;
    root : string option; [@default None] [@yojson_drop_default ( = )]
    locale : string option; [@default None] [@yojson_drop_default ( = )]
    symbols_to_index : string list option;
        [@default None] [@yojson_drop_default ( = )]
  }
  [@@deriving yojson_of]

  type create_synonyms_response = {
    id : string;
    synonyms : string list;
    root : string option; [@default None] [@yojson_drop_default ( = )]
    locale : string option; [@default None] [@yojson_drop_default ( = )]
    symbols_to_index : string list option;
        [@default None] [@yojson_drop_default ( = )]
  }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let create ~config ~collection_name ~synonym_id ~synonyms =
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/synonyms/" ^ Uri.pct_encode synonym_id
    in
    let body = synonyms |> yojson_of_synonyms |> Yojson.Safe.to_string in
    RequestDescriptor.post ~config ~body path

  type get_synonyms_response = create_synonyms_response [@@deriving of_yojson]

  let get ~config ~collection_name ~synonym_id =
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/synonyms/" ^ Uri.pct_encode synonym_id
    in
    RequestDescriptor.get ~config path

  type list_synonyms_response = { synonyms : get_synonyms_response list }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let list ~config ~collection_name =
    let path = "/collections/" ^ Uri.pct_encode collection_name ^ "/synonyms" in
    RequestDescriptor.get ~config path

  type delete_synonyms_response = { id : string }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let delete ~config ~collection_name ~synonym_id =
    let path =
      "/collections/"
      ^ Uri.pct_encode collection_name
      ^ "/synonyms/" ^ Uri.pct_encode synonym_id
    in
    RequestDescriptor.delete ~config path
end

module Cluster_operations = struct
  type cluster_operation_response = { success : bool }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let create_snapshot ~config ~snapshot_path =
    let path = "/operations/snapshot" in
    let params = [ ("snapshot_path", [ snapshot_path ]) ] in
    RequestDescriptor.post ~config ~params path

  let compact_db ~config =
    let path = "/operations/db/compact" in
    RequestDescriptor.post ~config path

  let re_elect_leader ~config =
    let path = "/operations/vote" in
    RequestDescriptor.post ~config path

  let toggle_slow_request_log ~config ?(log_slow_requests_time_ms = -1) () =
    let path = "/config" in
    let body =
      `Assoc [ ("log-slow-requests-time-ms", `Int log_slow_requests_time_ms) ]
      |> Yojson.Safe.to_string
    in
    RequestDescriptor.post ~config ~body path

  type metrics_response = {
    system_cpu1_active_percentage : string;
    system_cpu2_active_percentage : string;
    system_cpu3_active_percentage : string;
    system_cpu4_active_percentage : string;
    system_cpu_active_percentage : string;
    system_disk_total_bytes : string;
    system_disk_used_bytes : string;
    system_memory_total_bytes : string;
    system_memory_used_bytes : string;
    system_network_received_bytes : string;
    system_network_sent_bytes : string;
    typesense_memory_active_bytes : string;
    typesense_memory_allocated_bytes : string;
    typesense_memory_fragmentation_ratio : string;
    typesense_memory_mapped_bytes : string;
    typesense_memory_metadata_bytes : string;
    typesense_memory_resident_bytes : string;
    typesense_memory_retained_bytes : string;
  }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let get_metrics ~config =
    let path = "/metrics.json" in
    RequestDescriptor.get ~config path

  module StatsTable = struct
    type t = (string, float) Hashtbl.t

    let t_of_yojson v =
      match v with
      | `Assoc l ->
          let tbl = Hashtbl.create (List.length l) in
          let decode_kv_pair = function
            | k, `Float v -> Hashtbl.add tbl k v
            | _ -> raise (Invalid_argument "expected a float value")
          in
          l |> List.iter decode_kv_pair;
          tbl
      | _ ->
          raise (Invalid_argument "StatsTable.t_of_yojson expected an object")
  end

  type stats_response = {
    latency_ms : StatsTable.t;
    requests_per_second : StatsTable.t;
  }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let get_stats ~config =
    let path = "/stats.json" in
    RequestDescriptor.get ~config path

  type health_response = { ok : bool }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let get_health ~config =
    let path = "/health" in
    RequestDescriptor.get ~config ~headers:[] path
end

type request_error =
  [ `BadRequest
  | `Unauthorized
  | `NotFound
  | `Conflict
  | `UnprocessableEntity
  | `ServiceUnavailable ]
