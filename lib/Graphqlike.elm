module Graphqlike exposing (backend, sendSubscriptionData)

import Browser
import Browser.Navigation as Nav
import Graphqlike.Internal as I
import Graphqlike.Sub
import Lamdera exposing (ClientId, SessionId)
import Query exposing (Query)
import Url



{- TODO:
   - [ ] wrap the model in a type that includes the last thing sent to each ClientId
   - [ ] on each update, run the subscriptions, diff against the old thing, and
         send a ToFrontend msg if it's different
   - [ ] on each updateFromFrontend, do the same
-}


type alias Config backendModel backendMsg toBackendMsg toFrontendMsg =
    -- The new thing:
    { frontendSubscriptions : Graphqlike.Sub.Sub backendModel toFrontendMsg toBackendMsg backendMsg
    , lamderaBroadcast : toFrontendMsg -> Cmd backendMsg
    , lamderaSendToFrontend : ClientId -> toFrontendMsg -> Cmd backendMsg
    , clientIds : backendModel -> List ClientId

    -- Rest is as usual:
    , init : ( backendModel, Cmd backendMsg )
    , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
    , updateFromFrontend : SessionId -> ClientId -> toBackendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
    , subscriptions : backendModel -> Sub backendMsg
    }


backend :
    Config backendModel backendMsg toBackendMsg toFrontendMsg
    ->
        { init : ( backendModel, Cmd backendMsg )
        , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
        , updateFromFrontend : SessionId -> ClientId -> toBackendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
        , subscriptions : backendModel -> Sub backendMsg
        }
backend cfg =
    Lamdera.backend
        { init = cfg.init
        , update = update cfg
        , updateFromFrontend = updateFromFrontend cfg
        , subscriptions = cfg.subscriptions
        }


type BackendBoundMsg backendMsg toBackendMsg
    = BackendMsg backendMsg
    | ToBackendMsg toBackendMsg


update :
    Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> backendMsg
    -> backendModel
    -> ( backendModel, Cmd backendMsg )
update cfg msg model =
    let
        ( newModel, cmd ) =
            cfg.update msg model
    in
    ( newModel, cmd )
        |> handleFrontendSubscriptions cfg (BackendMsg msg) cfg.frontendSubscriptions


updateFromFrontend :
    Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> SessionId
    -> ClientId
    -> toBackendMsg
    -> backendModel
    -> ( backendModel, Cmd backendMsg )
updateFromFrontend cfg sessionId clientId msg model =
    let
        ( newModel, cmd ) =
            cfg.updateFromFrontend sessionId clientId msg model
    in
    ( newModel, cmd )
        -- TODO figure out which parts are clientId-specific and only run _those_ in a client-iterating loop
        -- TODO for the rest, it can be done once and reused for every client
        |> handleFrontendSubscriptions cfg (ToBackendMsg msg) cfg.frontendSubscriptions


{-| This is helpful eg. when first hydrating clients on ClientConnected, as it
will send the ToFrontend msgs unconditionally. The user is expected to wire
this through manually.
-}
sendSubscriptionData :
    backendModel
    -> ClientId
    -> (ClientId -> toFrontendMsg -> Cmd backendMsg)
    -> Graphqlike.Sub.Sub backendModel toFrontendMsg toBackendMsg backendMsg
    -> Cmd backendMsg
sendSubscriptionData model clientId lamderaSendToFrontend sub =
    case sub of
        I.Query _ query ->
            --  ^ We send things unconditionally so we don't use this config
            unconditionalQueryCmd lamderaSendToFrontend clientId query model

        I.Batch [] ->
            Cmd.none

        I.Batch subs ->
            subs
                |> List.map (sendSubscriptionData model clientId lamderaSendToFrontend)
                |> Cmd.batch


handleFrontendSubscriptions :
    Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> BackendBoundMsg backendMsg toBackendMsg
    -> Graphqlike.Sub.Sub backendModel toFrontendMsg toBackendMsg backendMsg
    -> ( backendModel, Cmd backendMsg )
    -> ( backendModel, Cmd backendMsg )
handleFrontendSubscriptions cfg msg sub ( model, cmd ) =
    case sub of
        I.Query cfg_ query ->
            case msg of
                BackendMsg backendMsg ->
                    if cfg_.fireAfterBackendMsg backendMsg then
                        ( model
                        , Cmd.batch
                            [ cmd
                            , queryCmd cfg query model
                            ]
                        )

                    else
                        ( model, cmd )

                ToBackendMsg toBackendMsg ->
                    if cfg_.fireAfterToBackendMsg toBackendMsg then
                        ( model
                        , Cmd.batch
                            [ cmd
                            , queryCmd cfg query model
                            ]
                        )

                    else
                        ( model, cmd )

        I.Batch [] ->
            ( model, cmd )

        I.Batch (aSub :: subs) ->
            ( model, cmd )
                |> handleFrontendSubscriptions cfg msg aSub
                |> handleFrontendSubscriptions cfg msg (I.Batch subs)


queryCmd :
    { config
        | clientIds : backendModel -> List ClientId
        , lamderaSendToFrontend : ClientId -> toFrontendMsg -> Cmd backendMsg
        , lamderaBroadcast : toFrontendMsg -> Cmd backendMsg
    }
    -- Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> Query backendModel toFrontendMsg
    -> backendModel
    -> Cmd backendMsg
queryCmd cfg query model =
    if I.usesClientId query then
        queryCmdWithClientId cfg query model

    else
        queryCmdWithoutClientId cfg query model


{-| Go over all clientIds and run the query for each.
TODO use CPS or something to only run the smallest clientId-sensitive part in this loop, and run the rest just once?
-}
queryCmdWithClientId :
    { config
        | clientIds : backendModel -> List ClientId
        , lamderaSendToFrontend : ClientId -> toFrontendMsg -> Cmd backendMsg
    }
    -> Query backendModel toFrontendMsg
    -> backendModel
    -> Cmd backendMsg
queryCmdWithClientId cfg query model =
    cfg.clientIds model
        |> List.map
            (\clientId ->
                -- This query is guaranteed to return Ok, because of the mapping we do in G.Sub
                case Query.run clientId model query of
                    Err err ->
                        -- Won't happen.
                        crash "queryCmdWithClientId - can't happen because G.Sub.query wraps stuff in Ok"

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
    { config | lamderaBroadcast : toFrontendMsg -> Cmd backendMsg }
    -> Query backendModel toFrontendMsg
    -> backendModel
    -> Cmd backendMsg
queryCmdWithoutClientId cfg query model =
    -- This query is guaranteed to return Ok, because of the mapping we do in G.Sub
    case Query.run "this client ID literally doesn't matter" model query of
        Err err ->
            -- Won't happen.
            crash "queryCmdWithoutClientId - can't happen because G.Sub.query wraps stuff in Ok"

        Ok toFrontendMsg ->
            -- TODO check if it's different from the last time we sent it
            -- TODO maybe via hashing?
            -- TODO and if it's the same, don't send it anymore!
            cfg.lamderaBroadcast toFrontendMsg


unconditionalQueryCmd :
    (ClientId -> toFrontendMsg -> Cmd backendMsg)
    -> ClientId
    -> Query backendModel toFrontendMsg
    -> backendModel
    -> Cmd backendMsg
unconditionalQueryCmd lamderaSendToFrontend clientId query model =
    case Query.run clientId model query of
        Err err ->
            -- Won't happen.
            crash "unconditionalQueryCmd - can't happen because G.Sub.query wraps stuff in Ok"

        Ok toFrontendMsg ->
            -- No checking against last-sent version because this is the first one!
            lamderaSendToFrontend clientId toFrontendMsg


crash : String -> a
crash why =
    let
        overflowTheStack x =
            1 + overflowTheStack x

        _ =
            overflowTheStack 0
    in
    crash why
