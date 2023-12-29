let by_field ?(descending = true) field_name =
  let order = if descending then "desc" else "asc" in
  Printf.sprintf "%s:%s" field_name order

(** Geopoint sorting *)

type by_geopoint =
  | RadiusMi of {
      lat : float;
      lon : float;
      exclude_radius_mi : float option;
      precision_mi : float option;
    }
  | RadiusKm of {
      lat : float;
      lon : float;
      exclude_radius_km : float option;
      precision_km : float option;
    }

let by_geopoint ~field_name s =
  let order = "asc" in
  match s with
  | RadiusMi { lat; lon; exclude_radius_mi; precision_mi } -> (
      match (exclude_radius_mi, precision_mi) with
      | None, None ->
          Printf.sprintf "%s(lat: %f, lon: %f):%s" field_name lat lon order
      | Some e, None ->
          Printf.sprintf "%s(lat: %f, lon: %f, exclude_radius: %fmi):%s"
            field_name lat lon e order
      | None, Some p ->
          Printf.sprintf "%s(lat: %f, lon: %f, precision: %fmi):%s" field_name
            lat lon p order
      | Some e, Some p ->
          Printf.sprintf
            "%s(lat: %f, lon: %f, exclude_radius: %fmi, precision: %fmi):%s"
            field_name lat lon e p order)
  | RadiusKm { lat; lon; exclude_radius_km; precision_km } -> (
      match (exclude_radius_km, precision_km) with
      | None, None ->
          Printf.sprintf "%s(lat: %f, lon: %f):%s" field_name lat lon order
      | Some e, None ->
          Printf.sprintf "%s(lat: %f, lon: %f, exclude_radius: %fkm):%s"
            field_name lat lon e order
      | None, Some p ->
          Printf.sprintf "%s(lat: %f, lon: %f, precision: %fkm):%s" field_name
            lat lon p order
      | Some e, Some p ->
          Printf.sprintf
            "%s(lat: %f, lon: %f, exclude_radius: %fkm, precision: %fkm):%s"
            field_name lat lon e p order)
