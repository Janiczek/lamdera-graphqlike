module Queries exposing
    ( Query
    , Sub
    , choice
    , choiceClientPoints
    , choiceName
    , choiceProgress
    , choiceTotalProgress
    , choices
    , completedQuest
    , completedQuests
    , quest
    , questBestChoice
    , questBestChoiceProgress
    , questChoices
    , questCompleted
    , questName
    , questThreshold
    , questWinningChoice
    , quests
    , subscriptions
    )

import Dict exposing (Dict)
import Graphqlike.Sub
import Lamdera exposing (ClientId)
import Query
import Set exposing (Set)
import Types
    exposing
        ( BackendModel
        , ChoiceId
        , CompletedQuest
        , QuestId
        , ToFrontend(..)
        )


type alias Query a =
    Query.Query BackendModel a


type alias Sub =
    -- TODO perhaps keep the `msg` around, for innerMsgs etc.
    -- would need Sub.map as well
    Graphqlike.Sub.Sub BackendModel ToFrontend



-- SUBSCRIPTIONS


subscriptions : Sub
subscriptions =
    Graphqlike.Sub.batch
        [ Graphqlike.Sub.query completedQuests GotCompletedQuests
        ]



-- COMPLETED QUEST


completedQuests : Query (List CompletedQuest)
completedQuests =
    quests completedQuest
        |> Query.map (List.filterMap identity)


completedQuest : QuestId -> Query (Maybe CompletedQuest)
completedQuest questId =
    questWinningChoice questId
        |> Query.andThen
            (\maybeChoiceId ->
                case maybeChoiceId of
                    Nothing ->
                        Query.succeed Nothing

                    Just choiceId ->
                        Query.succeed CompletedQuest
                            |> Query.andMap (Query.succeed questId)
                            |> Query.andMap (questName questId)
                            |> Query.andMap (questThreshold questId)
                            |> Query.andMap (Query.succeed choiceId)
                            |> Query.andMap (choiceName choiceId)
                            |> Query.andMap (choiceClientPoints choiceId)
                            |> Query.map Just
            )



-- QUEST


quests : (QuestId -> Query a) -> Query (List a)
quests f =
    Query.toplevel .quests
        |> Query.andThen
            (\questsDict ->
                questsDict
                    |> Dict.keys
                    |> Query.traverse f
            )


quest : QuestId -> Query { name : String, threshold : Int }
quest questId =
    Query.toplevel .quests
        |> Query.andThen (Query.dictGet questId)


questChoices : QuestId -> Query (Set ChoiceId)
questChoices questId =
    Query.toplevel .questChoices
        |> Query.andThen (Query.dictGet questId)


questThreshold : QuestId -> Query Int
questThreshold questId =
    quest questId
        |> Query.map .threshold


questBestChoice : QuestId -> Query (Maybe ( ChoiceId, Int ))
questBestChoice questId =
    questChoices questId
        |> Query.andThen
            (\choiceIds ->
                choiceIds
                    |> Set.toList
                    |> Query.traverse
                        (\choiceId ->
                            choiceTotalProgress choiceId
                                |> Query.map (Tuple.pair choiceId)
                        )
                    |> Query.map
                        (\choiceIdsWithPoints ->
                            choiceIdsWithPoints
                                |> List.sortBy (Tuple.second >> negate)
                                |> List.head
                        )
            )


questBestChoiceProgress : QuestId -> Query Int
questBestChoiceProgress questId =
    questBestChoice questId
        |> Query.map (Maybe.map Tuple.second >> Maybe.withDefault 0)


questCompleted : QuestId -> Query Bool
questCompleted questId =
    Query.map2 (\threshold progress -> progress >= threshold)
        (questThreshold questId)
        (questBestChoiceProgress questId)


questName : QuestId -> Query String
questName questId =
    quest questId
        |> Query.map .name


questWinningChoice : QuestId -> Query (Maybe ChoiceId)
questWinningChoice questId =
    Query.map2
        (\threshold maybeBestChoice ->
            case maybeBestChoice of
                Nothing ->
                    Nothing

                Just ( bestChoiceId, bestChoicePoints ) ->
                    if bestChoicePoints >= threshold then
                        Just bestChoiceId

                    else
                        Nothing
        )
        (questThreshold questId)
        (questBestChoice questId)



-- CHOICE


choices : (ChoiceId -> Query a) -> Query (List a)
choices f =
    Query.toplevel .choices
        |> Query.andThen
            (\choicesDict ->
                choicesDict
                    |> Dict.keys
                    |> Query.traverse f
            )


choice : ChoiceId -> Query { name : String }
choice choiceId =
    Query.toplevel .choices
        |> Query.andThen (Query.dictGet choiceId)


choiceName : ChoiceId -> Query String
choiceName choiceId =
    choice choiceId
        |> Query.map .name



-- CHOICE PROGRESS


choiceProgress : ChoiceId -> Query (Dict ClientId Int)
choiceProgress choiceId =
    Query.toplevel .choiceProgress
        |> Query.andThen (Query.dictGet choiceId)


choiceTotalProgress : ChoiceId -> Query Int
choiceTotalProgress choiceId =
    choiceProgress choiceId
        |> Query.map (Dict.foldl (\_ progress acc -> acc + progress) 0)


choiceClientPoints : ChoiceId -> Query Int
choiceClientPoints choiceId =
    Query.andThen2 (\progressDict clientId -> Query.dictGet clientId progressDict)
        (choiceProgress choiceId)
        Query.clientId
