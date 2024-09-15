module Graphqlike.Sub exposing (Sub, batch, none, query)

import Query as Q
import Query.Error as QE


type Sub backendModel msg
    = Query (Q.Query backendModel msg)
    | Batch (List (Sub backendModel msg))


none : Sub bm msg
none =
    Batch []


batch : List (Sub bm msg) -> Sub bm msg
batch list =
    Batch list


query : Q.Query bm a -> (Result QE.Error a -> msg) -> Sub bm msg
query q f =
    Query (\cid bm -> Ok (f (q cid bm)))
