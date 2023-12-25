open Ppx_yojson_conv_lib.Yojson_conv

type field_type =
  | String
  | StringArray
  | Int32
  | Int32Array
  | Int64
  | Int64Array
  | Float
  | FloatArray
  | Bool
  | BoolArray
  | Geopoint
  | GeopointArray
  | Object
  | ObjectArray
  | AutoConvertToString
  | Auto

let yojson_of_field_type = function
  | String -> `String "string"
  | StringArray -> `String "string[]"
  | Int32 -> `String "int32"
  | Int32Array -> `String "int32[]"
  | Int64 -> `String "int64"
  | Int64Array -> `String "int64[]"
  | Float -> `String "float"
  | FloatArray -> `String "float[]"
  | Bool -> `String "bool"
  | BoolArray -> `String "bool[]"
  | Geopoint -> `String "geopoint"
  | GeopointArray -> `String "geopoint[]"
  | Object -> `String "object"
  | ObjectArray -> `String "object[]"
  | AutoConvertToString -> `String "string*"
  | Auto -> `String "auto"

let field_type_of_yojson = function
  | `String "string" -> String
  | `String "string[]" -> StringArray
  | `String "int32" -> Int32
  | `String "int32[]" -> Int32Array
  | `String "int64" -> Int64
  | `String "int64[]" -> Int64Array
  | `String "float" -> Float
  | `String "float[]" -> FloatArray
  | `String "bool" -> Bool
  | `String "bool[]" -> BoolArray
  | `String "geopoint" -> Geopoint
  | `String "geopoint[]" -> GeopointArray
  | `String "object" -> Object
  | `String "object[]" -> ObjectArray
  | `String "string*" -> AutoConvertToString
  | `String "auto" -> Auto
  | _ -> raise (Invalid_argument "failed to decode field_type")

type field = {
  name : string;
  typesense_type : field_type;
  facet : bool;
  optional : bool;
  index : bool;
  locale : string option;
  sort : bool;
  drop : bool;
  infix : bool;
}
[@@deriving yojson]

let field ?(facet = false) ?(optional = false) ?(index = true) ?(locale = None)
    ?(sort = false) ?(drop = false) ?(infix = false) name type_ =
  {
    name;
    typesense_type = type_;
    facet;
    optional;
    index;
    locale;
    sort;
    drop;
    infix;
  }

type t = {
  name : string;
  fields : field list;
  token_separators : string list;
  symbols_to_index : string list;
  default_sorting_field : string option;
}
[@@deriving yojson]

let schema ?(token_separators = []) ?(symbols_to_index = [])
    ?(default_sorting_field = None) name fields =
  { name; fields; token_separators; symbols_to_index; default_sorting_field }
