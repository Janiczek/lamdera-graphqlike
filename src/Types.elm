module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Query.Error
import RemoteData exposing (RemoteData)
import Set exposing (Set)
import Url exposing (Url)



-- COMMON


type alias QuestId =
    String


type alias ChoiceId =
    String



-- BACKEND


type alias BackendModel =
    { clientIds : Set ClientId
    , graphqlikeCache : Dict ( String, ClientId ) Int
    , quests : Dict QuestId { name : String, threshold : Int }
    , choices : Dict ChoiceId { name : String }
    , questChoices : Dict QuestId (Set ChoiceId)
    , choicePoints : Dict ChoiceId (Dict ClientId Int)
    }


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | SaveToGraphqlikeCache ( String, ClientId ) Int



-- FRONTEND


type alias CompletedQuest =
    { id : QuestId
    , name : String
    , threshold : Int
    , winningChoiceId : ChoiceId
    , winningChoiceName : String
    , myContributionToWinningChoice : Int
    }


type alias OngoingQuest =
    { id : QuestId
    , name : String
    , threshold : Int
    , choices :
        List
            { id : ChoiceId
            , name : String
            , points : Int
            , myContribution : Int
            }
    }


type alias FrontendModel =
    { key : Key
    , completedQuests : RemoteData (List CompletedQuest)
    , ongoingQuests : RemoteData (List OngoingQuest)
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | InitQuestsClicked
    | AddQuestProgressClicked
    | UnrelatedMsgClicked



-- COMMUNICATION


type ToBackend
    = InitQuests
    | AddQuestProgress
    | UnrelatedMsg


type ToFrontend
    = GotCompletedQuests (Result Query.Error.Error (List CompletedQuest))
    | GotOngoingQuests (Result Query.Error.Error (List OngoingQuest))
