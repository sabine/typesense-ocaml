type action = Upsert | Update

type typesense_document_response = {
  success : bool;
  code : int32 option;
  error : string option;
  document : string; (* deserialize to JSON value *)
}
