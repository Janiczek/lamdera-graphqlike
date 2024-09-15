module Graphqlike exposing (backend)

import Browser
import Browser.Navigation as Nav
import Graphqlike.Sub
import Lamdera exposing (ClientId, SessionId)
import Url



{- TODO:
   - [ ] wrap the model in a type that includes the last thing sent to each ClientId
   - [ ] on each update, run the subscriptions, diff against the old thing, and send a ToFrontend msg if it's different
   - [ ] on each updateFromFrontend, do the same
-}


backend :
    -- The new thing:
    { frontendSubscriptions : Graphqlike.Sub.Sub model toFrontendMsg

    -- Rest is as usual:
    , init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , updateFromFrontend : SessionId -> ClientId -> toBackendMsg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    ->
        { init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , updateFromFrontend : SessionId -> ClientId -> toBackendMsg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        }
backend cfg =
    -- TODO use frontendSubscriptions
    Lamdera.backend
        { init = cfg.init
        , update =
            \msg model ->
                cfg.update msg model
                    |> handleFrontendSubscriptions cfg.frontendSubscriptions
        , updateFromFrontend =
            \sessionId clientId msg model ->
                cfg.updateFromFrontend sessionId clientId msg model
                    |> handleFrontendSubscriptions cfg.frontendSubscriptions
        , subscriptions = cfg.subscriptions
        }


handleFrontendSubscriptions :
    Graphqlike.Sub.Sub model toFrontendMsg
    -> ( model, Cmd msg )
    -> ( model, Cmd msg )
handleFrontendSubscriptions sub ( model, cmd ) =
    -- TODO do something
    ( model, cmd )
