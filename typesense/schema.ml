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

type create_field = {
  name : string;
  typesense_type : field_type; [@key "type"]
  optional : bool; [@default false] [@yojson_drop_default ( = )]
  facet : bool; [@default false] [@yojson_drop_default ( = )]
  index : bool; [@default true] [@yojson_drop_default ( = )]
  locale : string; [@default ""] [@yojson_drop_default ( = )]
}
[@@deriving yojson_of]

let create_field ?(facet = false) ?(optional = false) ?(index = true)
    ?(locale = "") name typesense_type =
  { name; typesense_type; facet; optional; index; locale }

type create_schema = {
  name : string;
  fields : create_field list;
  token_separators : string list; [@default []] [@yojson_drop_default ( = )]
  symbols_to_index : string list; [@default []] [@yojson_drop_default ( = )]
  default_sorting_field : string; [@default ""] [@yojson_drop_default ( = )]
}
[@@deriving yojson_of]

(* updating schema fields *)

type drop_schema_field = { name : string; drop : bool } [@@deriving yojson_of]
type update_schema_field = Drop of string | Add of create_field

let yojson_of_update_schema_field = function
  | Drop name -> yojson_of_drop_schema_field { name; drop = true }
  | Add field -> yojson_of_create_field field

let schema ?(token_separators = []) ?(symbols_to_index = [])
    ?(default_sorting_field = "") name fields =
  { name; fields; token_separators; symbols_to_index; default_sorting_field }

type update_schema = { fields : update_schema_field list }
[@@deriving yojson_of]

let update_schema fields = { fields }

(* listing collection *)

type field = {
  name : string;
  typesense_type : field_type; [@key "type"]
  optional : bool;
  facet : bool;
  index : bool;
  locale : string;
  sort : bool;
  infix : bool;
}
[@@deriving of_yojson]

type collection = {
  name : string;
  fields : field list;
  token_separators : string list; [@default []]
  symbols_to_index : string list; [@default []]
  default_sorting_field : string; [@default ""]
}
[@@deriving of_yojson]
