type field_type =
  | String
  | StringArray
  | Int32
  | Int32Array
  | Int64
  | Int64Array
  | Float
  | FloatArray
  | Geopoint
  | GeopointArray
  | Object
  | ObjectArray
  | AutoConvertToString
  | Auto

type field = {
  name : string;
  typesense_type : field_type;
  optional : bool;
  facet : bool;
  index : bool;
  locale : string option;
  sort : bool;
  drop : bool;
  infix : bool;
}

type t = {
  name : string;
  fields : field list;
  token_separators : string list;
  symbols_to_index : string list;
  default_sorting_field : string option;
}
