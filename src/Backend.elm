module Backend exposing (..)

import Dict
import Graphqlike
import Lamdera exposing (ClientId, SessionId)
import Queries
import Set
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Graphqlike.backend
        -- The following are just like in Lamdera.backend:
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions

        -- Here's the new stuff.
        , frontendSubscriptions = Queries.subscriptions
        , lamderaBroadcast = Lamdera.broadcast
        , lamderaSendToFrontend = Lamdera.sendToFrontend
        , typesW3EncodeToFrontend = Types.w3_encode_ToFrontend
        , clientIds = .clientIds >> Set.toList
        , cache = .graphqlikeCache
        , saveToCache = SaveToGraphqlikeCache
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { graphqlikeCache = Dict.empty
      , clientIds = Set.empty
      , counts = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected _ clientId ->
            let
                newModel =
                    { model | clientIds = Set.insert clientId model.clientIds }
            in
            ( newModel
            , Cmd.batch
                [ Graphqlike.sendSubscriptionData
                    newModel
                    clientId
                    Lamdera.sendToFrontend
                    Queries.subscriptions
                , Lamdera.sendToFrontend clientId (HelloYouAre clientId)
                ]
            )

        ClientDisconnected _ clientId ->
            ( { model | clientIds = Set.remove clientId model.clientIds }
            , Cmd.none
            )

        SaveToGraphqlikeCache key hash ->
            ( { model
                | graphqlikeCache =
                    model.graphqlikeCache
                        |> Dict.insert key hash
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        Increment ->
            let
                newCount =
                    model.counts
                        |> Dict.get clientId
                        |> Maybe.withDefault 0
                        |> (+) 1
            in
            ( { model
                | counts =
                    model.counts
                        |> Dict.insert clientId newCount
              }
            , -- Look, no derivation of leaderboard here, but it will still get sent if changed!
              -- We can still do normal Lamdera BE<->FE communication as well: let's send the user their new score.
              Lamdera.sendToFrontend clientId (IncrementAck newCount)
            )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
