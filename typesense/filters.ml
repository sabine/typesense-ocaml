module StringFilter = struct
  type t =
    | Match of { q : string }
    | ExactMatch of { q : string }
    | NotEquals of { q : string }
    | MatchOneOf of { q : string list }
    | ExactMatchOneOf of { q : string list }
    | ExactMatchNoneOf of { q : string list }

  let to_string f =
    match f with
    | Match { q } -> q
    | ExactMatch { q } -> "=" ^ q
    | NotEquals { q } -> "!=" ^ q
    | MatchOneOf { q } -> "[" ^ String.concat "," q ^ "]"
    | ExactMatchOneOf { q } -> "=[" ^ String.concat "," q ^ "]"
    | ExactMatchNoneOf { q } -> "!=[" ^ String.concat "," q ^ "]"
end

module MakeNumberFilter (Number : sig
  type t

  val to_string : t -> string
end) =
struct
  type number_range = { min : Number.t; max : Number.t }
  type number_value_or_range = Number of Number.t | Range of number_range

  let string_of_number_value_or_range (f : number_value_or_range) =
    match f with
    | Number n -> Number.to_string n
    | Range r ->
        "[" ^ Number.to_string r.min ^ ".." ^ Number.to_string r.max ^ "]"

  type t =
    | Match of { q : Number.t }
    | Less of { q : Number.t }
    | LessOrEqual of { q : Number.t }
    | Greater of { q : Number.t }
    | GreaterOrEqual of { q : Number.t }
    | MatchOneOf of { q : number_value_or_range list }

  let to_string (f : t) =
    match f with
    | Match { q } -> Number.to_string q
    | Less { q } -> "<" ^ Number.to_string q
    | LessOrEqual { q } -> "<=" ^ Number.to_string q
    | Greater { q } -> ">" ^ Number.to_string q
    | GreaterOrEqual { q } -> ">=" ^ Number.to_string q
    | MatchOneOf { q } ->
        "["
        ^ String.concat "," (List.map string_of_number_value_or_range q)
        ^ "]"
end

module Int64Filter = MakeNumberFilter (Int64)
module Int32Filter = MakeNumberFilter (Int32)
module FloatFilter = MakeNumberFilter (Float)

module GeopointFilter = struct
  type geopoint = { lat : Float.t; lon : Float.t }

  let string_of_geopoint geopoint =
    Float.to_string geopoint.lat ^ "," ^ Float.to_string geopoint.lon

  type t =
    | InRadiusAroundLocationKm of {
        geopoint : geopoint;
        distance_in_km : Float.t;
      }
    | InRadiusAroundLocationMi of {
        geopoint : geopoint;
        distance_in_mi : Float.t;
      }
    | InsideGeoPolygon of { geopoints : geopoint list }

  let string_of_t = function
    | InRadiusAroundLocationKm { geopoint; distance_in_km } ->
        "("
        ^ string_of_geopoint geopoint
        ^ ","
        ^ Float.to_string distance_in_km
        ^ " km) "
    | InRadiusAroundLocationMi { geopoint; distance_in_mi } ->
        "("
        ^ string_of_geopoint geopoint
        ^ ","
        ^ Float.to_string distance_in_mi
        ^ " mi) "
    | InsideGeoPolygon { geopoints } ->
        "(" ^ String.concat "," (List.map string_of_geopoint geopoints) ^ ")"
end
