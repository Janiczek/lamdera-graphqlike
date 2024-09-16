module Graphqlike exposing (backend)

import Browser
import Browser.Navigation as Nav
import Graphqlike.Internal as I
import Graphqlike.Sub
import Lamdera exposing (ClientId, SessionId)
import Query exposing (Query)
import Types exposing (ToFrontend)
import Url



{- TODO:
   - [ ] wrap the model in a type that includes the last thing sent to each ClientId
   - [ ] on each update, run the subscriptions, diff against the old thing, and send a ToFrontend msg if it's different
   - [ ] on each updateFromFrontend, do the same
-}


type alias Config model msg toBackendMsg toFrontendMsg =
    -- The new thing:
    { frontendSubscriptions : Graphqlike.Sub.Sub model toFrontendMsg
    , lamderaBroadcast : toFrontendMsg -> Cmd msg
    , lamderaSendToFrontend : ClientId -> toFrontendMsg -> Cmd msg
    , clientIds : model -> List ClientId

    -- Rest is as usual:
    , init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , updateFromFrontend : SessionId -> ClientId -> toBackendMsg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }


backend :
    Config model msg toBackendMsg toFrontendMsg
    ->
        { init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , updateFromFrontend : SessionId -> ClientId -> toBackendMsg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        }
backend cfg =
    Lamdera.backend
        { init = cfg.init
        , update = update cfg
        , updateFromFrontend = updateFromFrontend cfg
        , subscriptions = cfg.subscriptions
        }


update :
    Config model msg toBackendMsg toFrontendMsg
    -> msg
    -> model
    -> ( model, Cmd msg )
update cfg msg model =
    let
        ( newModel, cmd ) =
            cfg.update msg model
    in
    ( newModel, cmd )
        |> handleFrontendSubscriptions cfg cfg.frontendSubscriptions


updateFromFrontend :
    Config model msg toBackendMsg toFrontendMsg
    -> SessionId
    -> ClientId
    -> toBackendMsg
    -> model
    -> ( model, Cmd msg )
updateFromFrontend cfg sessionId clientId msg model =
    let
        ( newModel, cmd ) =
            cfg.updateFromFrontend sessionId clientId msg model
    in
    ( newModel, cmd )
        -- TODO figure out which parts are clientId-specific and only run _those_ in a client-iterating loop
        -- TODO for the rest, it can be done once and reused for every client
        |> handleFrontendSubscriptions cfg cfg.frontendSubscriptions


handleFrontendSubscriptions :
    Config model msg toBackendMsg toFrontendMsg
    -> Graphqlike.Sub.Sub model toFrontendMsg
    -> ( model, Cmd msg )
    -> ( model, Cmd msg )
handleFrontendSubscriptions cfg sub ( model, cmd ) =
    case sub of
        I.Query query ->
            ( model
            , Cmd.batch
                [ cmd
                , queryCmd cfg query model
                ]
            )

        I.Batch [] ->
            ( model, cmd )

        I.Batch (aSub :: subs) ->
            ( model, cmd )
                |> handleFrontendSubscriptions cfg aSub
                |> handleFrontendSubscriptions cfg (I.Batch subs)


queryCmd :
    Config model msg toBackendMsg toFrontendMsg
    -> Query model toFrontendMsg
    -> model
    -> Cmd msg
queryCmd cfg query model =
    if I.usesClientId query then
        queryCmdWithClientId cfg query model

    else
        queryCmdWithoutClientId cfg query model


{-| Go over all clientIds and run the query for each.
TODO use CPS or something to only run the smallest clientId-sensitive part in this loop, and run the rest just once?
-}
queryCmdWithClientId :
    Config model msg toBackendMsg toFrontendMsg
    -> Query model toFrontendMsg
    -> model
    -> Cmd msg
queryCmdWithClientId cfg query model =
    cfg.clientIds model
        |> List.map
            (\clientId ->
                -- This query is guaranteed to return Ok, because of the mapping we do in G.Sub
                case Query.run clientId model query of
                    Err err ->
                        -- Won't happen.
                        crash "queryCmdWithClientId"

                    Ok toFrontendMsg ->
                        -- TODO check if it's different from the last time we sent it
                        -- TODO maybe via hashing?
                        -- TODO and if it's the same, don't send it anymore!
                        cfg.lamderaSendToFrontend clientId toFrontendMsg
            )
        |> Cmd.batch


{-| The query doesn't use clientId, so we can put arbitrary string in there and
it won't change the results.

This also means we don't need to loop over clientIds and run the query for
each. We can run it just once and broadcast!

-}
queryCmdWithoutClientId :
    Config model msg toBackendMsg toFrontendMsg
    -> Query model toFrontendMsg
    -> model
    -> Cmd msg
queryCmdWithoutClientId cfg query model =
    -- This query is guaranteed to return Ok, because of the mapping we do in G.Sub
    case Query.run "this client ID literally doesn't matter" model query of
        Err err ->
            -- Won't happen.
            crash "queryCmdWithoutClientId"

        Ok toFrontendMsg ->
            -- TODO check if it's different from the last time we sent it
            -- TODO maybe via hashing?
            -- TODO and if it's the same, don't send it anymore!
            cfg.lamderaBroadcast toFrontendMsg


crash : String -> a
crash why =
    let
        overflowTheStack x =
            1 + overflowTheStack x

        _ =
            overflowTheStack 0
    in
    crash why
