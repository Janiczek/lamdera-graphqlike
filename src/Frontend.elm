module Frontend exposing (..)

import Browser
import Html
import Html.Attributes
import Html.Events
import Lamdera
import Queries as Q
import Query.Error
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = \_ _ -> init
        , onUrlRequest = \_ -> DontCareAboutBrowserNav
        , onUrlChange = \_ -> DontCareAboutBrowserNav
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( { onlineUsers = 0
      , leaderboard = []
      , myClientId = ""
      , myCount = 0
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        DontCareAboutBrowserNav ->
            ( model, Cmd.none )

        IncrementClicked ->
            ( model, Lamdera.sendToBackend Increment )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        HelloYouAre clientId ->
            ( { model | myClientId = clientId }
            , Cmd.none
            )

        GotOnlineUsers (Err err) ->
            -- Ignoring this for the example. But Queries can fail in general.
            ( model, Cmd.none )

        GotOnlineUsers (Ok onlineUsers) ->
            ( { model | onlineUsers = onlineUsers }
            , Cmd.none
            )

        GotLeaderboard (Err err) ->
            -- Ignoring this for the example. But Queries can fail in general.
            ( model, Cmd.none )

        GotLeaderboard (Ok leaderboard) ->
            ( { model | leaderboard = leaderboard }
            , Cmd.none
            )

        IncrementAck newCount ->
            ( { model | myCount = newCount }
            , Cmd.none
            )


c : String -> Html.Attribute msg
c class =
    Html.Attributes.class class


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Graphqlike example - Multiplayer Counter"
    , body =
        [ Html.div [ c "bg-slate-100 h-screen p-8 flex flex-col gap-6" ]
            [ Html.h1
                [ c "font-semibold text-4xl text-blue-600" ]
                [ Html.text "Graphqlike example - Multiplayer Counter" ]
            , Html.div []
                [ Html.text "This example shows off:"
                , Html.ul [ c "list-disc pl-8" ]
                    [ Html.li [] [ Html.text "how to set Graphqlike up in a Lamdera project" ]
                    , Html.li [] [ Html.text "basic queries (check the bottom of the Queries module)" ]
                    , Html.li [] [ Html.text "how derived data (the online users count and the leaderboard) is automatically sent to all frontends whenever it changes" ]
                    ]
                ]
            , Html.div [ c "flex flex-row gap-4" ]
                [ card "bg-blue-500"
                    "About you"
                    [ Html.code
                        [ c "text-base text-white opacity-75" ]
                        [ Html.text model.myClientId ]
                    , Html.span
                        [ c "text-base text-white opacity-75" ]
                        [ Html.text <| "Score: " ++ String.fromInt model.myCount ]
                    ]
                , card "bg-orange-500"
                    "Online users"
                    [ Html.span
                        [ c "text-base text-white opacity-75" ]
                        [ Html.text <| String.fromInt model.onlineUsers ]
                    ]
                ]
            , Html.div [ c "flex flex-col gap-2" ]
                [ Html.h2
                    [ c "font-semibold text-2xl text-blue-500" ]
                    [ Html.text "Leaderboard (best 5)" ]
                , (model.leaderboard
                    |> List.map
                        (\( clientId, msgCount ) ->
                            Html.li
                                [ if clientId == model.myClientId then
                                    c "bg-lime-300 w-fit"

                                  else
                                    Html.Attributes.classList []
                                ]
                                [ Html.code [] [ Html.text clientId ]
                                , Html.text <| ": " ++ String.fromInt msgCount
                                ]
                        )
                  )
                    |> Html.ol [ c "list-decimal pl-8" ]
                ]
            , Html.div []
                [ Html.button
                    [ c "text-white font-bold px-6 py-4 rounded outline-none focus:outline-none bg-blue-700 active:bg-blue-600 text-sm shadow hover:shadow-lg"
                    , Html.Events.onClick IncrementClicked
                    ]
                    [ Html.text <| "Increment (" ++ String.fromInt model.myCount ++ ")" ]
                ]
            ]
        ]
    }


card : String -> String -> List (Html.Html msg) -> Html.Html msg
card bg title content =
    Html.section
        [ c (bg ++ " shadow-lg rounded-lg text-center p-8 flex flex-col w-fit") ]
        (Html.span
            [ c "text-lg text-white font-semibold" ]
            [ Html.text title ]
            :: content
        )
