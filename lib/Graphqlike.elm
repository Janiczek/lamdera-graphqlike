module Graphqlike exposing (frontend)

import Browser
import Browser.Navigation as Nav
import Graphqlike.Sub
import Lamdera
import Url


frontend :
    -- The new thing:
    { backendSubscriptions : model -> Graphqlike.Sub.Sub backendModel msg

    -- Rest is as usual:
    , init : Url.Url -> Nav.Key -> ( model, Cmd msg )
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    , update : msg -> model -> ( model, Cmd msg )
    , updateFromBackend : toFrontendMsg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Browser.Document msg
    }
    ->
        { init : Url.Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlRequest : Browser.UrlRequest -> msg
        , onUrlChange : Url.Url -> msg
        , update : msg -> model -> ( model, Cmd msg )
        , updateFromBackend : toFrontendMsg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Browser.Document msg
        }
frontend cfg =
    -- TODO use backendSubscriptions
    Lamdera.frontend
        { init = cfg.init
        , onUrlRequest = cfg.onUrlRequest
        , onUrlChange = cfg.onUrlChange
        , update = cfg.update
        , updateFromBackend = cfg.updateFromBackend
        , subscriptions = cfg.subscriptions
        , view = cfg.view
        }
