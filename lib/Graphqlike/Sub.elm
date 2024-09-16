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
    String
    -> (Result QE.Error a -> toFrontendMsg)
    -> Q.Query backendModel a
    -> Sub backendModel toFrontendMsg toBackendMsg backendMsg
query cacheKey tagger (I.Q info query_) =
    I.QuerySub
        { fireAfterBackendMsg = \_ -> True
        , fireAfterToBackendMsg = \_ -> True
        , cacheKey = cacheKey
        }
        (I.Q info
            (\clientId backendModel ->
                Ok (tagger (query_ clientId backendModel))
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
