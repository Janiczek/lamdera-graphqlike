module Graphqlike.Sub exposing (Sub, batch, none, query)

import Graphqlike.Internal as I
import Query as Q
import Query.Error as QE


type alias Sub backendModel msg =
    I.Sub backendModel msg


none : Sub bm msg
none =
    I.Batch []


batch : List (Sub bm msg) -> Sub bm msg
batch list =
    I.Batch list


query : Q.Query bm a -> (Result QE.Error a -> msg) -> Sub bm msg
query q f =
    I.Query (\cid bm -> Ok (f (q cid bm)))
