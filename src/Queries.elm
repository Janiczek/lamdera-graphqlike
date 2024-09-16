module Queries exposing
    ( DataSub
    , Query
    , subscriptions
    )

import Dict exposing (Dict)
import Dict.Extra
import Graphqlike
import Graphqlike.Sub
import Lamdera exposing (ClientId)
import Query
import Set exposing (Set)
import Types
    exposing
        ( BackendModel
        , BackendMsg(..)
        , ToBackend(..)
        , ToFrontend(..)
        )


type alias Query a =
    Query.Query BackendModel a


type alias DataSub =
    Graphqlike.Sub.Sub BackendModel ToFrontend ToBackend BackendMsg



-- SUBSCRIPTIONS


subscriptions : List DataSub
subscriptions =
    [ onlineUsersSub
    , leaderboardSub
    ]


onlineUsersSub : DataSub
onlineUsersSub =
    Graphqlike.Sub.query
        { cacheKey = "online-users"
        , toMsg = GotOnlineUsers
        , query = onlineUsers
        }
        |> Graphqlike.Sub.fireOnlyAfterSpecificBackendMsgs
            (\msg ->
                case msg of
                    ClientConnected _ _ ->
                        True

                    ClientDisconnected _ _ ->
                        True

                    SaveToGraphqlikeCache _ _ ->
                        False
            )
        |> Graphqlike.Sub.fireOnlyAfterSpecificToBackendMsgs
            (\msg ->
                case msg of
                    Increment ->
                        False
            )


leaderboardSub : DataSub
leaderboardSub =
    Graphqlike.Sub.query
        { cacheKey = "leaderboard"
        , toMsg = GotLeaderboard
        , query = leaderboard
        }
        |> Graphqlike.Sub.fireOnlyAfterSpecificBackendMsgs
            (\msg ->
                case msg of
                    ClientConnected _ _ ->
                        False

                    ClientDisconnected _ _ ->
                        False

                    SaveToGraphqlikeCache _ _ ->
                        False
            )
        |> Graphqlike.Sub.fireOnlyAfterSpecificToBackendMsgs
            (\msg ->
                case msg of
                    Increment ->
                        True
            )



-- QUERIES


onlineUsers : Query Int
onlineUsers =
    Query.toplevel .clientIds
        |> Query.map Set.size


leaderboard : Query (List ( ClientId, Int ))
leaderboard =
    Query.map2
        (\clientIds counts ->
            let
                -- We want the leaderboard to show users who didn't send a
                -- message yet as well. So let's init the leaderboard with all
                -- 0s and then merge the actual counts into it.
                emptyCounts : Dict ClientId Int
                emptyCounts =
                    clientIds
                        |> Set.toList
                        |> List.map (\clientId -> ( clientId, 0 ))
                        |> Dict.fromList
            in
            Dict.union counts emptyCounts
                |> Dict.toList
                |> List.sortBy (Tuple.second >> negate)
                |> List.take 5
        )
        (Query.toplevel .clientIds)
        (Query.toplevel .counts)
