module Graphqlike.Internal exposing (Query(..), Sub(..), usesClientId)

import Lamdera exposing (ClientId)
import Query.Error exposing (Error)


type Query m a
    = Q { usesClientId : Bool } (ClientId -> m -> Result Error a)


usesClientId : Query m a -> Bool
usesClientId (Q info _) =
    info.usesClientId


type Sub backendModel msg
    = Query (Query backendModel msg)
    | Batch (List (Sub backendModel msg))
