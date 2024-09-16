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

        UnrelatedMsgClicked ->
            ( model, Lamdera.sendToBackend UnrelatedMsg )


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


c : String -> Html.Attribute msg
c class =
    Html.Attributes.class class


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Graphqlike example - Quests"
    , body =
        [ Html.div [ c "bg-slate-100 h-screen p-8 flex flex-col gap-6" ]
            [ Html.h1
                [ c "font-semibold text-4xl text-blue-600" ]
                [ Html.text "Graphqlike example - Quests" ]
            , Html.div []
                [ Html.text "This example shows off:"
                , Html.ul [ c "list-disc pl-8" ]
                    [ Html.li [] [ Html.text "more involved queries that span across multiple \"columns in a database\" (check the Queries module)" ]
                    , Html.li []
                        [ Html.code [] [ Html.text "clientId" ]
                        , Html.text "-aware queries ("
                        , Html.code [] [ Html.text "Queries.choiceClientPoints" ]
                        , Html.text ")"
                        ]
                    , Html.li []
                        [ Html.text "skipping sending an update to frontend when the data didn't change ("
                        , Html.code [] [ Html.text "UnrelatedMsg" ]
                        , Html.text " won't result in a ToFrontend msg being sent even though the data got recalculated, check the console)"
                        ]
                    ]
                ]
            , Html.div [ c "flex flex-row gap-2" ]
                [ button
                    [ Html.Events.onClick InitQuestsClicked
                    , Html.Attributes.disabled (model.ongoingQuests /= Success [])
                    ]
                    "Add quests"
                , button
                    [ Html.Events.onClick AddQuestProgressClicked
                    , Html.Attributes.disabled (model.ongoingQuests == Success [])
                    ]
                    "Add quest progress"
                , button
                    [ Html.Events.onClick UnrelatedMsgClicked ]
                    "Unrelated msg"
                ]
            , Html.div [ c "flex flex-row gap-2" ]
                [ Html.div [ c "flex-1 flex flex-col gap-4" ]
                    [ Html.h2
                        [ c "font-semibold text-2xl text-blue-500" ]
                        [ Html.text "Completed quests" ]
                    , remoteDataView model.completedQuests <|
                        \completedQuests ->
                            if List.isEmpty completedQuests then
                                Html.span
                                    [ c "text-gray-500" ]
                                    [ Html.text "No completed quests yet." ]

                            else
                                completedQuests
                                    |> List.map
                                        (\quest ->
                                            Html.li []
                                                [ Html.span [ c "font-semibold text-lg text-blue-500" ] [ Html.text quest.name ]
                                                , Html.ul [ c "flex flex-col gap-2" ]
                                                    [ Html.li
                                                        [ c "bg-indigo-500 text-white text-xs font-bold uppercase px-3 py-1 rounded outline-none focus:outline-none w-fit" ]
                                                        [ Html.text <| "Threshold: " ++ String.fromInt quest.threshold ]
                                                    , Html.li
                                                        [ c "w-3/12 p-4 flex flex-col min-w-0 break-words bg-white rounded shadow-lg w-fit gap-2" ]
                                                        [ Html.span [ c "text-blue-400 uppercase font-bold text-xs" ] [ Html.text "Winning choice" ]
                                                        , Html.ul []
                                                            [ Html.li [] [ Html.text quest.winningChoiceName ]
                                                            , Html.li [] [ Html.text <| "My contribution: " ++ String.fromInt quest.myContributionToWinningChoice ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                        )
                                    |> Html.ul [ c "flex flex-row gap-4" ]
                    ]
                , Html.div [ c "flex-1 flex flex-col gap-4" ]
                    [ Html.h2
                        [ c "font-semibold text-2xl text-blue-500" ]
                        [ Html.text "Ongoing quests" ]
                    , remoteDataView model.ongoingQuests <|
                        \ongoingQuests ->
                            ongoingQuests
                                |> List.map
                                    (\quest ->
                                        Html.li []
                                            [ Html.span
                                                [ c "font-semibold text-lg text-blue-500" ]
                                                [ Html.text quest.name ]
                                            , Html.ul [ c "flex flex-col gap-2" ]
                                                [ Html.li
                                                    [ c "bg-indigo-500 text-white text-xs font-bold uppercase px-3 py-1 rounded outline-none focus:outline-none w-fit" ]
                                                    [ Html.text <| "Threshold: " ++ String.fromInt quest.threshold ]
                                                , Html.li []
                                                    [ quest.choices
                                                        |> List.map
                                                            (\choice ->
                                                                Html.li [ c "w-3/12 p-4 flex flex-col min-w-0 break-words bg-white rounded shadow-lg gap-2" ]
                                                                    [ Html.span
                                                                        [ c "text-blue-400 uppercase font-bold text-xs" ]
                                                                        [ Html.text choice.name ]
                                                                    , Html.ul []
                                                                        [ Html.li [] [ Html.text <| "Total points: " ++ String.fromInt choice.points ]
                                                                        , Html.li [] [ Html.text <| "My contribution: " ++ String.fromInt choice.myContribution ]
                                                                        ]
                                                                    ]
                                                            )
                                                        |> Html.ul [ c "flex flex-wrap gap-2" ]
                                                    ]
                                                ]
                                            ]
                                    )
                                |> Html.ul [ c "flex flex-col gap-4" ]
                    ]
                ]
            ]
        ]
    }


button : List (Html.Attribute msg) -> String -> Html.Html msg
button attrs text =
    Html.button
        (c "text-white font-bold px-6 py-4 rounded outline-none focus:outline-none bg-blue-700 active:bg-blue-600 text-sm shadow hover:shadow-lg disabled:bg-gray-400 disabled:hover:shadow"
            :: attrs
        )
        [ Html.text text ]
