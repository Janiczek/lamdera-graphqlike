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

        --
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
      , quests = Dict.empty
      , choices = Dict.empty
      , questChoices = Dict.empty
      , choicePoints = Dict.empty
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
            , Graphqlike.sendSubscriptionData
                newModel
                clientId
                Lamdera.sendToFrontend
                Queries.subscriptions
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
        InitQuests ->
            let
                quests =
                    Dict.fromList
                        [ ( "quest1", { name = "Quest 1", threshold = 100 } )
                        , ( "quest2", { name = "Second quest", threshold = 50 } )
                        ]

                choices =
                    quests
                        |> Dict.keys
                        |> List.concatMap
                            (\questId ->
                                [ ( questId ++ "_choice1", { name = "Choice 1" } )
                                , ( questId ++ "_choice2", { name = "Another choice" } )
                                , ( questId ++ "_choice3", { name = "And the last choice" } )
                                ]
                            )
                        |> Dict.fromList

                questChoices =
                    quests
                        |> Dict.map
                            (\questId _ ->
                                Set.fromList
                                    [ questId ++ "_choice1"
                                    , questId ++ "_choice2"
                                    , questId ++ "_choice3"
                                    ]
                            )

                choicePoints =
                    choices
                        |> Dict.map (\_ _ -> Dict.empty)
            in
            ( { model
                | quests = quests
                , choices = choices
                , questChoices = questChoices
                , choicePoints = choicePoints
              }
            , Cmd.none
            )

        AddQuestProgress ->
            ( { model
                | choicePoints =
                    model.choicePoints
                        |> Dict.map
                            (\_ dict ->
                                dict
                                    |> Dict.update clientId
                                        (Maybe.withDefault 0
                                            >> (+) 10
                                            >> Just
                                        )
                            )
              }
            , Cmd.none
            )

        UnrelatedMsg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]
