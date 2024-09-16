module Graphqlike exposing (backend, sendSubscriptionData)

import Browser
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Bytes.Encode
import Dict exposing (Dict)
import FNV1a
import Graphqlike.Internal as I
import Graphqlike.Sub
import Hex.Convert
import Lamdera exposing (ClientId, SessionId)
import Lamdera.Wire3
import Query exposing (Query)
import Task
import Url



{- TODO:
   - [ ] wrap the model in a type that includes the last thing sent to each ClientId
   - [ ] on each update, run the subscriptions, diff against the old thing, and
         send a ToFrontend msg if it's different
   - [ ] on each updateFromFrontend, do the same
-}


type alias Config backendModel backendMsg toBackendMsg toFrontendMsg =
    -- The new thing:
    { frontendSubscriptions : List (Graphqlike.Sub.Sub backendModel toFrontendMsg toBackendMsg backendMsg)
    , lamderaBroadcast : toFrontendMsg -> Cmd backendMsg
    , lamderaSendToFrontend : ClientId -> toFrontendMsg -> Cmd backendMsg
    , typesW3EncodeToFrontend : toFrontendMsg -> Bytes.Encode.Encoder
    , saveToCache : ( String, ClientId ) -> Int -> backendMsg
    , cache : backendModel -> Dict ( String, ClientId ) Int
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
    ( newModel
    , Cmd.batch
        [ cmd
        , sendSubscriptionDataIfChanged cfg (BackendMsg msg) cfg.frontendSubscriptions newModel
        ]
    )


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
    ( newModel
    , Cmd.batch
        [ cmd
        , -- TODO figure out which parts are clientId-specific and only run _those_ in a client-iterating loop
          -- TODO for the rest, it can be done once and reused for every client
          sendSubscriptionDataIfChanged cfg (ToBackendMsg msg) cfg.frontendSubscriptions newModel
        ]
    )


{-| This is helpful eg. when first hydrating clients on ClientConnected, as it
will send the ToFrontend msgs unconditionally. The user is expected to wire
this through manually.
-}
sendSubscriptionData :
    Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> backendModel
    -> ClientId
    -> List (Graphqlike.Sub.Sub backendModel toFrontendMsg toBackendMsg backendMsg)
    -> Cmd backendMsg
sendSubscriptionData cfg model clientId subs =
    subs
        |> List.map
            (\(I.QuerySub { cacheKey } query) ->
                unconditionalQueryCmd cfg cacheKey clientId query model
            )
        |> Cmd.batch


sendSubscriptionDataIfChanged :
    Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> BackendBoundMsg backendMsg toBackendMsg
    -> List (Graphqlike.Sub.Sub backendModel toFrontendMsg toBackendMsg backendMsg)
    -> backendModel
    -> Cmd backendMsg
sendSubscriptionDataIfChanged cfg msg subs model =
    subs
        |> List.map
            (\(I.QuerySub cfg_ query) ->
                case msg of
                    BackendMsg backendMsg ->
                        if cfg_.fireAfterBackendMsg backendMsg then
                            queryCmd cfg_.cacheKey cfg query model

                        else
                            Cmd.none

                    ToBackendMsg toBackendMsg ->
                        if cfg_.fireAfterToBackendMsg toBackendMsg then
                            queryCmd cfg_.cacheKey cfg query model

                        else
                            Cmd.none
            )
        |> Cmd.batch


queryCmd :
    String
    -> Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> Query backendModel toFrontendMsg
    -> backendModel
    -> Cmd backendMsg
queryCmd cacheKey cfg query model =
    if I.usesClientId query then
        queryCmdWithClientId cacheKey cfg query model

    else
        queryCmdWithoutClientId cacheKey cfg query model


{-| Go over all clientIds and run the query for each.
TODO use CPS or something to only run the smallest clientId-sensitive part in this loop, and run the rest just once?
-}
queryCmdWithClientId :
    String
    -> Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> Query backendModel toFrontendMsg
    -> backendModel
    -> Cmd backendMsg
queryCmdWithClientId cacheKey cfg query model =
    cfg.clientIds model
        |> List.map
            (\clientId ->
                -- This query is guaranteed to return Ok, because of the mapping we do in G.Sub
                case Query.run clientId model query of
                    Err err ->
                        -- Won't happen.
                        crash "queryCmdWithClientId - can't happen because G.Sub.query wraps stuff in Ok"

                    Ok toFrontendMsg ->
                        withCaching
                            cfg
                            ( cacheKey, clientId )
                            model
                            toFrontendMsg
                            (\() -> cfg.lamderaSendToFrontend clientId toFrontendMsg)
            )
        |> Cmd.batch


withCaching :
    Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> ( String, ClientId )
    -> backendModel
    -> toFrontendMsg
    -> (() -> Cmd backendMsg)
    -> Cmd backendMsg
withCaching cfg key model toFrontendMsg send =
    let
        currentHash : Int
        currentHash =
            toFrontendMsg
                |> cfg.typesW3EncodeToFrontend
                |> Lamdera.Wire3.bytesEncode
                |> Hex.Convert.toString
                |> FNV1a.hash

        sendAndSave () =
            Cmd.batch
                [ send ()
                , cfg.saveToCache key currentHash
                    |> Task.succeed
                    |> Task.perform identity
                ]
    in
    case Dict.get key (cfg.cache model) of
        Nothing ->
            -- Not in cache, send and save!
            sendAndSave ()

        Just oldHash ->
            if oldHash == currentHash then
                -- Skip!
                Cmd.none

            else
                sendAndSave ()


{-| The query doesn't use clientId, so we can put arbitrary string in there and
it won't change the results.

This also means we don't need to loop over clientIds and run the query for
each. We can run it just once and broadcast!

-}
queryCmdWithoutClientId :
    String
    -> Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> Query backendModel toFrontendMsg
    -> backendModel
    -> Cmd backendMsg
queryCmdWithoutClientId cacheKey cfg query model =
    -- This query is guaranteed to return Ok, because of the mapping we do in G.Sub
    case Query.run "this client ID literally doesn't matter" model query of
        Err err ->
            -- Won't happen.
            crash "queryCmdWithoutClientId - can't happen because G.Sub.query wraps stuff in Ok"

        Ok toFrontendMsg ->
            withCaching
                cfg
                ( cacheKey, "" )
                model
                toFrontendMsg
                (\() -> cfg.lamderaBroadcast toFrontendMsg)


unconditionalQueryCmd :
    Config backendModel backendMsg toBackendMsg toFrontendMsg
    -> String
    -> ClientId
    -> Query backendModel toFrontendMsg
    -> backendModel
    -> Cmd backendMsg
unconditionalQueryCmd cfg cacheKey clientId query model =
    case Query.run clientId model query of
        Err err ->
            -- Won't happen.
            crash "unconditionalQueryCmd - can't happen because G.Sub.query wraps stuff in Ok"

        Ok toFrontendMsg ->
            withCaching
                cfg
                ( cacheKey, clientId )
                model
                toFrontendMsg
                (\() -> cfg.lamderaSendToFrontend clientId toFrontendMsg)


crash : String -> a
crash why =
    let
        overflowTheStack x =
            1 + overflowTheStack x

        _ =
            overflowTheStack 0
    in
    crash why
