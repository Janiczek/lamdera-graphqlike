module Graphqlike.Internal exposing (Query(..), Sub(..), usesClientId)

import Lamdera exposing (ClientId)
import Query.Error exposing (Error)


type Query m a
    = Q { usesClientId : Bool } (ClientId -> m -> Result Error a)


usesClientId : Query m a -> Bool
usesClientId (Q info _) =
    info.usesClientId


type Sub backendModel toFrontendMsg toBackendMsg backendMsg
    = QuerySub
        { fireAfterBackendMsg : backendMsg -> Bool
        , fireAfterToBackendMsg : toBackendMsg -> Bool
        , cacheKey : String
        }
        (Query backendModel toFrontendMsg)
