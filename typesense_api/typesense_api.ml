module Make (Config : sig
  val api_key : string
  val url : string
end) (HttpClient : sig
  val get :
    headers:(string * string) list ->
    string ->
    (string, [> `Msg of string ]) result
end) =
struct
  module Config = Config

  let headers = [ ("X-TYPESENSE-API-KEY", Config.api_key) ]
  let collections () = HttpClient.get ~headers (Config.url ^ "/collections")
end
