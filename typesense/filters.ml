type string_filter =
  | Match of { q : string }
  | ExactMatch of { q : string }
  | NotEquals of { q : string }
  | MatchOneOf of { q : string list }
  | ExactMatchOneOf of { q : string list }

type string_list_filter = string_filter

type 'a number_filter =
  | Match of { q : 'a }
  | Less of { q : 'a }
  | LessOrEqual of { q : 'a }
  | Greater of { q : 'a }
  | GreaterOrEqual of { q : 'a }
  | InRange of { start : 'a; end_ : 'a }

type 'a number_list_filter = Match of { q : 'a }

(*TODO: number filter needs to be a functor

  pub type Int64Filter = NumberFilter<i64>;
  pub type Int32Filter = NumberFilter<i32>;
  pub type FloatFilter = NumberFilter<f32>;

  pub type Int64ListFilter = NumberListFilter<i64>;
  pub type Int32ListFilter = NumberListFilter<i64>;
  pub type FloatListFilter = NumberListFilter<i64>;
*)

type geopoint_filter =
  | InRadiusAroundLocation of {
      lat : float;
      lon : float;
      distance_in_km : float;
    }
