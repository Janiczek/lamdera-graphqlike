module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
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


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body = [ Html.text <| Debug.toString model ]
    }
