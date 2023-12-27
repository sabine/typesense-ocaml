module Make (Config : sig
  val api_key : string
  val url : string
end) =
struct
  module Schema = Typesense.Schema
  module Config = Config

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

    let get ?(headers = headers) ?(params = []) path =
      Get { host = Config.url; path; headers; params }

    let post ?(headers = headers) ?(params = []) ~body path =
      Post { host = Config.url; path; headers; params; body }

    let delete ?(headers = headers) ?(params = []) path =
      Delete { host = Config.url; path; headers; params }

    let patch ?(headers = headers) ?(params = []) ~body path =
      Patch { host = Config.url; path; headers; params; body }

    let put ?(headers = headers) ?(params = []) ~body path =
      Put { host = Config.url; path; headers; params; body }
  end
  (* TODO: model and enforce all the response types for these endpoints *)

  module Collection = struct
    let list () = RequestDescriptor.get "/collections"

    let create schema =
      let body =
        Typesense.Schema.yojson_of_create_schema schema |> Yojson.Safe.to_string
      in
      RequestDescriptor.post ~body "/collections"

    let get collection_name =
      let path = "/collections/" ^ Uri.pct_encode collection_name in
      RequestDescriptor.get path

    let clone existing_collection_name new_collection_name =
      let path = "/collections" in
      let params = [ ("src_name", [ existing_collection_name ]) ] in
      let body =
        Yojson.Safe.to_string (`Assoc [ ("name", `String new_collection_name) ])
      in
      RequestDescriptor.post ~params ~body path

    let delete collection_name =
      let path = "/collections/" ^ Uri.pct_encode collection_name in
      RequestDescriptor.delete path

    let update collection_name (update_schema : Typesense.Schema.update_schema)
        =
      let path = "/collections/" ^ Uri.pct_encode collection_name in
      let body =
        Typesense.Schema.yojson_of_update_schema update_schema
        |> Yojson.Safe.to_string
      in
      RequestDescriptor.patch ~body path

    module Alias = struct
      let create_or_update collection_name alias =
        let path = "/aliases/" ^ Uri.pct_encode collection_name in
        let body =
          Yojson.Safe.to_string (`Assoc [ ("collection_name", `String alias) ])
        in
        RequestDescriptor.put ~body path

      let get alias =
        let path = "/aliases/" ^ Uri.pct_encode alias in
        RequestDescriptor.get path

      let list () =
        let path = "/aliases" in
        RequestDescriptor.get path

      let delete alias =
        let path = "/aliases/" ^ Uri.pct_encode alias in
        RequestDescriptor.delete path
    end
  end

  module Document = struct
    (* TODO: enforce document type to match schema of the collection *)

    type dirty_values = Coerce_or_reject | Coerce_or_drop | Drop | Reject

    let string_of_dirty_values v =
      match v with
      | Coerce_or_reject -> "coerce_or_reject"
      | Coerce_or_drop -> "coerce_or_drop"
      | Drop -> "drop"
      | Reject -> "reject"

    let add ?dirty_values ?remote_embedding_timeout_ms
        ?remote_embedding_num_tries collection_name document =
      let body = document in
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/documents"
      in
      let params =
        let open Params in
        add_if_int "dirty_values" dirty_values
        @ add_if_int "remote_embedding_timeout_ms" remote_embedding_timeout_ms
        @ add_if_int "remote_embedding_num_tries" remote_embedding_num_tries
      in
      RequestDescriptor.post ~params ~body path

    let _import ?(batch_size = None) ?remote_embedding_timeout_ms
        ?remote_embedding_num_tries collection_name documents action =
      let body = String.concat "\n" documents in
      let params =
        let open Params in
        [ ("action", [ action ]) ]
        @ add_if_int "batch_size" batch_size
        @ add_if_int "remote_embedding_timeout_ms" remote_embedding_timeout_ms
        @ add_if_int "remote_embedding_num_tries" remote_embedding_num_tries
      in
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/documents/import"
      in
      RequestDescriptor.post ~params ~body path

    let import_create collection_name documents =
      _import collection_name documents "create"

    let import_upsert collection_name documents =
      _import collection_name documents "upsert"

    let import_update collection_name documents =
      _import collection_name documents "update"

    let import_emplace collection_name documents =
      _import collection_name documents "emplace"

    let retrieve collection_name document_id =
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/documents/" ^ document_id
      in
      RequestDescriptor.get path

    let update collection_name document_id document_patch =
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/documents/" ^ document_id
      in
      RequestDescriptor.patch ~body:document_patch path

    let update_by_query ~filter_by collection_name document_patch =
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/documents"
      in
      let params = [ ("filter_by", [ filter_by ]) ] in
      RequestDescriptor.patch ~params ~body:document_patch path

    let delete collection_name document_id =
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/documents/" ^ document_id
      in
      RequestDescriptor.delete path

    let delete_by_query ~filter_by collection_name =
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/documents"
      in
      let params = [ ("filter_by", [ filter_by ]) ] in
      RequestDescriptor.delete ~params path

    let export ?(filter_by = "") ?(include_fields = "") ?(exclude_fields = "")
        collection_name =
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
      RequestDescriptor.get ~params path
  end

  module Search = struct
    module SearchParams =
      struct
      [@ocamlformat "disable"]
      let search_params
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
    end

    let search ~search_params collection_name =
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/documents/search"
      in
      RequestDescriptor.get ~params:search_params path

    module MultiSearchRequest = struct
      open Ppx_yojson_conv_lib.Yojson_conv

      type t = {
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

      let make ~query_by
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
        ?(x_typesense_api_key="")
        ()
        =
        {
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
    end

    type multi_search_request = {
      searches : MultiSearchRequest.t;
      limit_multi_searches : bool; [@default false] [@yojson_drop_default ( = )]
    }

    let multi_search ~search_requests ~common_search_params collection_name =
      let body =
        search_requests |> MultiSearchRequest.yojson_of_t
        |> Yojson.Safe.to_string
      in
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/documents/multi-search"
      in
      RequestDescriptor.post ~params:common_search_params ~body path

    (* TODO: add calls that allow to bypass the typed params / body *)
  end
end
