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
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        , frontendSubscriptions = Queries.subscriptions
        , lamderaBroadcast = Lamdera.broadcast
        , lamderaSendToFrontend = Lamdera.sendToFrontend
        , clientIds = .clientIds >> Set.toList
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { clientIds = Set.empty
      , quests = Dict.empty
      , choices = Dict.empty
      , questChoices = Dict.empty
      , choiceProgress = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected _ clientId ->
            ( { model | clientIds = Set.insert clientId model.clientIds }
            , Cmd.none
            )

        ClientDisconnected _ clientId ->
            ( { model | clientIds = Set.remove clientId model.clientIds }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
