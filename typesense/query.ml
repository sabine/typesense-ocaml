type facet_count = { count : int32; value : string }
type facet = { field_name : string; counts : facet_count list }
type 'document hit = { document : 'document }

type 'document typesense_response = {
  facet_counts : (string, facet_count list) Hashtbl.t;
  found : int32;
  hits : 'document hit list;
  out_of : int32;
  page : int32;
  search_time_ms : int32;
}

(* needs to be functors *)

type 'filter query = {
  q : string;
  query_by_with_weights : (string * int32) list;
  filter_by : 'filter list;
  facet_by : string list;
  page : int32;
  per_page : int32;
  include_fields : string list;
  max_facet_values : int32;
  num_typos : int32;
  drop_tokens_threshold : int32;
  typo_tokens_threshold : int32;
}

let search (_schema : Schema.collection) (query : 'filter query) =
  let _query_by =
    query.query_by_with_weights
    |> List.map (fun (field_name, _) -> field_name)
    |> String.concat ","
  in
  let _query_by_weight =
    query.query_by_with_weights
    |> List.map (fun (_, weight) -> Int32.to_string weight)
    |> String.concat ","
  in
  let _facet_by = query.facet_by |> String.concat "," in

  (* filter_by *)
  let _include_fields = query.include_fields |> String.concat "," in
  failwith "not implemented"
