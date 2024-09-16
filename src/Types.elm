module Types exposing (..)

import Browser exposing (UrlRequest)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Query.Error
import RemoteData exposing (RemoteData)
import Set exposing (Set)
import Url exposing (Url)



-- BACKEND


type alias BackendModel =
    { clientIds : Set ClientId
    , graphqlikeCache : Dict ( String, ClientId ) Int
    , counts : Dict ClientId Int
    }


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | SaveToGraphqlikeCache ( String, ClientId ) Int



-- FRONTEND


type alias FrontendModel =
    { myClientId : String
    , onlineUsers : Int
    , leaderboard : List ( ClientId, Int )
    , myCount : Int
    }


type FrontendMsg
    = DontCareAboutBrowserNav
    | IncrementClicked



-- COMMUNICATION


type ToBackend
    = Increment


type ToFrontend
    = HelloYouAre ClientId
    | GotOnlineUsers (Result Query.Error.Error Int)
    | GotLeaderboard (Result Query.Error.Error (List ( ClientId, Int )))
    | IncrementAck Int
