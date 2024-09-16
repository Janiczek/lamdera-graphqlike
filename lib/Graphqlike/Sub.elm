module Graphqlike.Sub exposing
    ( Sub
    , fireOnlyAfterSpecificBackendMsgs
    , fireOnlyAfterSpecificToBackendMsgs
    , query
    )

import Graphqlike.Internal as I
import Query as Q
import Query.Error as QE


type alias Sub backendModel toFrontendMsg toBackendMsg backendMsg =
    I.Sub backendModel toFrontendMsg toBackendMsg backendMsg


query :
    { cacheKey : String
    , toMsg : Result QE.Error a -> toFrontendMsg
    , query : Q.Query backendModel a
    }
    -> Sub backendModel toFrontendMsg toBackendMsg backendMsg
query cfg =
    let
        (I.Q info query_) =
            cfg.query
    in
    I.QuerySub
        { fireAfterBackendMsg = \_ -> True
        , fireAfterToBackendMsg = \_ -> True
        , cacheKey = cfg.cacheKey
        }
        (I.Q info
            (\clientId backendModel ->
                Ok (cfg.toMsg (query_ clientId backendModel))
            )
        )


fireOnlyAfterSpecificBackendMsgs :
    (backendMsg -> Bool)
    -> Sub backendModel toFrontendMsg toBackendMsg backendMsg
    -> Sub backendModel toFrontendMsg toBackendMsg backendMsg
fireOnlyAfterSpecificBackendMsgs pred (I.QuerySub cfg q) =
    I.QuerySub { cfg | fireAfterBackendMsg = pred } q


fireOnlyAfterSpecificToBackendMsgs :
    (toBackendMsg -> Bool)
    -> Sub backendModel toFrontendMsg toBackendMsg backendMsg
    -> Sub backendModel toFrontendMsg toBackendMsg backendMsg
fireOnlyAfterSpecificToBackendMsgs pred (I.QuerySub cfg q) =
    I.QuerySub { cfg | fireAfterToBackendMsg = pred } q
