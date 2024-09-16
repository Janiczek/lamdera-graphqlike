module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes
import Html.Events
import Lamdera
import Queries as Q
import Query.Error
import RemoteData exposing (RemoteData(..))
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , completedQuests = Loading
      , ongoingQuests = Loading
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        InitQuestsClicked ->
            ( model, Lamdera.sendToBackend InitQuests )

        AddQuestProgressClicked ->
            ( model, Lamdera.sendToBackend AddQuestProgress )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        -- TODO find a way to smuggle `never` in there, so that we can have guarantee of non-fallible queries
        GotCompletedQuests (Err err) ->
            ( { model | completedQuests = Error err }
            , Cmd.none
            )

        GotCompletedQuests (Ok completedQuests) ->
            ( { model | completedQuests = Success completedQuests }
            , Cmd.none
            )

        GotOngoingQuests (Err err) ->
            ( { model | ongoingQuests = Error err }
            , Cmd.none
            )

        GotOngoingQuests (Ok ongoingQuests) ->
            ( { model | ongoingQuests = Success ongoingQuests }
            , Cmd.none
            )


remoteDataView : RemoteData a -> (a -> Html.Html msg) -> Html.Html msg
remoteDataView data viewSuccess =
    case data of
        Loading ->
            Html.text "Loading..."

        Error err ->
            Html.text <| "Error: " ++ Debug.toString err

        Success success ->
            viewSuccess success


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.h1 [] [ Html.text "Completed quests" ]
        , remoteDataView model.completedQuests <|
            \completedQuests ->
                completedQuests
                    |> List.map
                        (\quest ->
                            Html.li []
                                [ Html.text quest.name
                                , Html.ul []
                                    [ Html.li [] [ Html.text <| "Threshold: " ++ String.fromInt quest.threshold ]
                                    , Html.li [] [ Html.text <| "Winning choice: " ++ quest.winningChoiceName ]
                                    , Html.li [] [ Html.text <| "My contribution: " ++ String.fromInt quest.myContributionToWinningChoice ]
                                    ]
                                ]
                        )
                    |> Html.ul []
        , Html.h1 [] [ Html.text "Ongoing quests" ]
        , remoteDataView model.ongoingQuests <|
            \ongoingQuests ->
                ongoingQuests
                    |> List.map
                        (\quest ->
                            Html.li []
                                [ Html.text quest.name
                                , Html.ul []
                                    [ Html.li [] [ Html.text <| "Threshold: " ++ String.fromInt quest.threshold ]
                                    , Html.li []
                                        [ Html.text "Choices:"
                                        , quest.choices
                                            |> List.map
                                                (\choice ->
                                                    Html.li []
                                                        [ Html.text choice.name
                                                        , Html.ul []
                                                            [ Html.li [] [ Html.text <| "Total points: " ++ String.fromInt choice.points ]
                                                            , Html.li [] [ Html.text <| "My contribution: " ++ String.fromInt choice.myContribution ]
                                                            ]
                                                        ]
                                                )
                                            |> Html.ul []
                                        ]
                                    ]
                                ]
                        )
                    |> Html.ul []
        , Html.div []
            [ Html.button
                [ Html.Events.onClick InitQuestsClicked
                , Html.Attributes.disabled (model.ongoingQuests /= Success [])
                ]
                [ Html.text "Add quests" ]
            , Html.button
                [ Html.Events.onClick AddQuestProgressClicked
                , Html.Attributes.disabled (model.ongoingQuests == Success [])
                ]
                [ Html.text "Add quest progress" ]
            ]
        ]
    }
