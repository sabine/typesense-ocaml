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
      let make
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
        ?(x_typesense_api_key="")
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
        @ add_if_string "x-typesense-api-key" x_typesense_api_key
    end

    let search ?(x_typesense_user_id = "") ~search_params collection_name =
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/documents/search"
      in
      let headers =
        if x_typesense_user_id <> "" then
          ("X-TYPESENSE-USER-ID", x_typesense_user_id)
          :: RequestDescriptor.headers
        else RequestDescriptor.headers
      in
      RequestDescriptor.get ~params:search_params ~headers path

    module MultiSearch = struct
      open Ppx_yojson_conv_lib.Yojson_conv

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

      type request = {
          searches : single_search list;
        } [@@deriving yojson_of]

      let perform ~search_requests ~common_search_params ?(x_typesense_user_id="") collection_name =
        let body =
          search_requests |> yojson_of_request
          |> Yojson.Safe.to_string
        in
        let path =
          "/collections/"
          ^ Uri.pct_encode collection_name
          ^ "/documents/multi-search"
        in
        let headers =
          if x_typesense_user_id <> "" then
          ("X-TYPESENSE-USER-ID", x_typesense_user_id) :: RequestDescriptor.headers
          else RequestDescriptor.headers
        in
        RequestDescriptor.post ~params:common_search_params ~body ~headers path
    end
  end

  module Analytics = struct
    let create_rule ~name ~rule_type ~source_collections ~destination_collection
        ~limit =
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
                          `List
                            (List.map (fun s -> `String s) source_collections)
                        );
                      ] );
                  ( "destination",
                    `Assoc [ ("collection", `String destination_collection) ] );
                  ("limit", limit);
                ] );
          ]
        |> Yojson.Safe.to_string
      in
      RequestDescriptor.post ~body path

    let list_rules =
      let path = "/analytics/rules" in
      RequestDescriptor.get path

    let delete_rule rule_name =
      let path = "/analytics/rules/" ^ Uri.pct_encode rule_name in
      RequestDescriptor.delete path
  end

  module Keys = struct
    open Ppx_yojson_conv_lib.Yojson_conv

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
    [@@deriving of_yojson]

    let create_key body =
      let path = "/keys" in
      let body = body |> yojson_of_create_key |> Yojson.Safe.to_string in
      RequestDescriptor.post ~body path

    type get_key_response = {
      id : int;
      actions : string list;
      collections : string list;
      description : string;
      value_prefix : string;
      expires_at : int64;
    }
    [@@deriving of_yojson]

    let get_key key_id =
      let path = "/keys/" ^ Uri.pct_encode (string_of_int key_id) in
      RequestDescriptor.get path

    type list_keys_response = { keys : get_key_response list }

    let list_keys =
      let path = "/keys" in
      RequestDescriptor.get path

    type delete_key_response = { id : int }

    let delete_key key_id =
      let path = "/keys/" ^ Uri.pct_encode (string_of_int key_id) in
      RequestDescriptor.delete path
  end

  module Override = struct
    open Ppx_yojson_conv_lib.Yojson_conv

    type override_rule = {
      query : string; [@default ""] [@yojson_drop_default ( = )]
      filter_by : string; [@default ""] [@yojson_drop_default ( = )]
      _match : string; [@key "match"] [@default ""] [@yojson_drop_default ( = )]
    }
    [@@deriving yojson]

    type override_include = { id : string; position : int } [@@deriving yojson]
    type override_exclude = { id : string } [@@deriving yojson]

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
      effective_to_ts : int64 option;
          [@default None] [@yojson_drop_default ( = )]
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
      effective_to_ts : int64 option;
          [@default None] [@yojson_drop_default ( = )]
      stop_processing : bool option; [@default None] [@yojson_drop_default ( = )]
    }
    [@@deriving of_yojson]

    let create ~collection_name ~override_id override =
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/overrides/" ^ Uri.pct_encode override_id
      in
      let body = override |> yojson_of_override |> Yojson.Safe.to_string in
      RequestDescriptor.post ~body path

    type list_override_response = { overrides : create_override_response list }
    [@@deriving of_yojson]

    let list ~collection_name =
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/overrides"
      in
      RequestDescriptor.get path

    type delete_override_response = { id : string } [@@deriving of_yojson]

    let delete ~collection_name ~override_id =
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/overrides/" ^ Uri.pct_encode override_id
      in
      RequestDescriptor.delete path
  end

  module Synonym = struct
    open Ppx_yojson_conv_lib.Yojson_conv

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
    [@@deriving of_yojson]

    let create ~collection_name ~synonym_id synonyms =
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/synonyms/" ^ Uri.pct_encode synonym_id
      in
      let body = synonyms |> yojson_of_synonyms |> Yojson.Safe.to_string in
      RequestDescriptor.post ~body path

    type get_synonyms_response = create_synonyms_response [@@deriving of_yojson]

    let get ~collection_name ~synonym_id =
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/synonyms/" ^ Uri.pct_encode synonym_id
      in
      RequestDescriptor.get path

    type list_synonyms_response = { synonyms : get_synonyms_response list }
    [@@deriving of_yojson]

    let list ~collection_name =
      let path =
        "/collections/" ^ Uri.pct_encode collection_name ^ "/synonyms"
      in
      RequestDescriptor.get path

    type delete_synonyms_response = { id : string } [@@deriving of_yojson]

    let delete ~collection_name ~synonym_id =
      let path =
        "/collections/"
        ^ Uri.pct_encode collection_name
        ^ "/synonyms/" ^ Uri.pct_encode synonym_id
      in
      RequestDescriptor.delete path
  end

  module Cluster_operations = struct
    open Ppx_yojson_conv_lib.Yojson_conv

    type cluster_operation_response = { success : bool } [@@deriving of_yojson]

    let create_snapshot ~snapshot_path =
      let path = "/operations/snapshot" in
      let params = [ ("snapshot_path", [ snapshot_path ]) ] in
      RequestDescriptor.post ~params path

    let compact_db =
      let path = "/operations/db/compact" in
      RequestDescriptor.post path

    let re_elect_leader =
      let path = "/operations/vote" in
      RequestDescriptor.post path

    let toggle_slow_request_log ~log_slow_requests_time_ms =
      let path = "/config" in
      let body =
        `Assoc [ ("log-slow-requests-time-ms", `Int log_slow_requests_time_ms) ]
        |> Yojson.Safe.to_string
      in
      RequestDescriptor.post ~body path

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
    [@@deriving of_yojson]

    let get_metrics =
      let path = "/metrics.json" in
      RequestDescriptor.get path

    module Stats_table = struct
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
        | _ -> raise (Invalid_argument "Hashtbl.t_of_yojson failed")
    end

    type stats_response = {
      latency_ms : Stats_table.t;
      requests_per_second : Stats_table.t;
    }
    [@@deriving of_yojson]

    let get_stats =
      let path = "/stats.json" in
      RequestDescriptor.get path

    type health_response = { ok : bool } [@@deriving of_yojson]

    let get_health =
      let path = "/health" in
      RequestDescriptor.get path
  end

  type request_error =
    [ `BadRequest
    | `Unauthorized
    | `NotFound
    | `Conflict
    | `UnprocessableEntity
    | `ServiceUnavailable ]
end
