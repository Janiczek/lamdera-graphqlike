module Backend exposing (..)

import Dict
import Graphqlike
import Lamdera exposing (ClientId, SessionId)
import Queries
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Graphqlike.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        , frontendSubscriptions = Queries.subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { quests = Dict.empty
      , choices = Dict.empty
      , questChoices = Dict.empty
      , choiceProgress = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )
