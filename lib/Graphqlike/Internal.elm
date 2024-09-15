module Graphqlike.Internal exposing (Sub(..))

import Query as Q


type Sub backendModel msg
    = Query (Q.Query backendModel msg)
    | Batch (List (Sub backendModel msg))
