module Graphqlike.Sub exposing
    ( Sub
    , batch
    , none
    , query
    )

import Graphqlike.Internal as I
import Query as Q
import Query.Error as QE


type alias Sub backendModel toFrontendMsg toBackendMsg backendMsg =
    I.Sub backendModel toFrontendMsg toBackendMsg backendMsg


none : Sub backendModel toFrontendMsg toBackendMsg backendMsg
none =
    I.Batch []


batch : List (Sub backendModel toFrontendMsg toBackendMsg backendMsg) -> Sub backendModel toFrontendMsg toBackendMsg backendMsg
batch list =
    I.Batch list


query :
    { toToFrontendMsg : Result QE.Error a -> toFrontendMsg
    , fireAfterBackendMsg : backendMsg -> Bool
    , fireAfterToBackendMsg : toBackendMsg -> Bool
    , cacheKey : String
    }
    -> Q.Query backendModel a
    -> Sub backendModel toFrontendMsg toBackendMsg backendMsg
query cfg (I.Q info query_) =
    I.Query
        { fireAfterBackendMsg = cfg.fireAfterBackendMsg
        , fireAfterToBackendMsg = cfg.fireAfterToBackendMsg
        , cacheKey = cfg.cacheKey
        }
        (I.Q info
            (\clientId backendModel ->
                Ok (cfg.toToFrontendMsg (query_ clientId backendModel))
            )
        )
