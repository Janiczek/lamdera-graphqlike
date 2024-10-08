module Queries exposing
    ( DataSub
    , Query
    , dataSubscriptions
    )

import Dict exposing (Dict)
import Graphqlike
import Graphqlike.Sub
import Lamdera exposing (ClientId)
import Query
import Set exposing (Set)
import Types
    exposing
        ( BackendModel
        , BackendMsg(..)
        , ChoiceId
        , CompletedQuest
        , OngoingQuest
        , QuestId
        , ToBackend(..)
        , ToFrontend(..)
        )


type alias Query a =
    Query.Query BackendModel a


type alias DataSub =
    -- TODO perhaps keep the `msg` around, for innerMsgs etc.
    -- would need Sub.map as well
    Graphqlike.Sub.Sub BackendModel ToFrontend ToBackend BackendMsg



-- SUBSCRIPTIONS


dataSubscriptions : List DataSub
dataSubscriptions =
    [ completedQuestsSub
    , ongoingQuestsSub
    ]


completedQuestsSub : DataSub
completedQuestsSub =
    Graphqlike.Sub.query
        { cacheKey = "completed-quests"
        , toMsg = GotCompletedQuests
        , query = completedQuests
        }
        |> Graphqlike.Sub.fireOnlyAfterSpecificBackendMsgs completedQuestsFireAfterBackendMsg
        |> Graphqlike.Sub.fireOnlyAfterSpecificToBackendMsgs completedQuestsFireAfterToBackendMsg


ongoingQuestsSub : DataSub
ongoingQuestsSub =
    Graphqlike.Sub.query
        { cacheKey = "ongoing-quests"
        , toMsg = GotOngoingQuests
        , query = ongoingQuests
        }
        |> Graphqlike.Sub.fireOnlyAfterSpecificBackendMsgs ongoingQuestsFireAfterBackendMsg
        |> Graphqlike.Sub.fireOnlyAfterSpecificToBackendMsgs ongoingQuestsFireAfterToBackendMsg


completedQuestsFireAfterBackendMsg : BackendMsg -> Bool
completedQuestsFireAfterBackendMsg msg =
    case msg of
        ClientConnected _ _ ->
            False

        ClientDisconnected _ _ ->
            False

        SaveToGraphqlikeCache _ _ ->
            False


completedQuestsFireAfterToBackendMsg : ToBackend -> Bool
completedQuestsFireAfterToBackendMsg msg =
    case msg of
        InitQuests ->
            True

        AddQuestProgress ->
            True

        UnrelatedMsg ->
            -- This should normally be False, but we want to demonstrate how
            -- even though this True will cause the backend to run the queries
            -- needlessly, it _still_ won't send a ToFrontend msg to the client
            -- because the data didn't change. (We're hashing the ToFrontend msgs)
            True


ongoingQuestsFireAfterBackendMsg : BackendMsg -> Bool
ongoingQuestsFireAfterBackendMsg msg =
    case msg of
        ClientConnected _ _ ->
            False

        ClientDisconnected _ _ ->
            False

        SaveToGraphqlikeCache _ _ ->
            False


ongoingQuestsFireAfterToBackendMsg : ToBackend -> Bool
ongoingQuestsFireAfterToBackendMsg msg =
    case msg of
        InitQuests ->
            True

        AddQuestProgress ->
            True

        UnrelatedMsg ->
            False



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



-- ONGOING QUEST


ongoingQuests : Query (List OngoingQuest)
ongoingQuests =
    quests ongoingQuest
        |> Query.map (List.filterMap identity)


ongoingQuest : QuestId -> Query (Maybe OngoingQuest)
ongoingQuest questId =
    questWinningChoice questId
        |> Query.andThen
            (\maybeChoiceId ->
                case maybeChoiceId of
                    Nothing ->
                        Query.succeed OngoingQuest
                            |> Query.andMap (Query.succeed questId)
                            |> Query.andMap (questName questId)
                            |> Query.andMap (questThreshold questId)
                            |> Query.andMap
                                (questChoices questId
                                    |> Query.andThen
                                        (\choiceIds ->
                                            choiceIds
                                                |> Set.toList
                                                |> Query.traverse
                                                    (\choiceId ->
                                                        Query.succeed (\id name points myContribution -> { id = id, name = name, points = points, myContribution = myContribution })
                                                            |> Query.andMap (Query.succeed choiceId)
                                                            |> Query.andMap (choiceName choiceId)
                                                            |> Query.andMap (choiceTotalPoints choiceId)
                                                            |> Query.andMap (choiceClientPoints choiceId)
                                                    )
                                        )
                                )
                            |> Query.map Just

                    Just choiceId ->
                        Query.succeed Nothing
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
                            choiceTotalPoints choiceId
                                |> Query.map (Tuple.pair choiceId)
                        )
                    |> Query.map
                        (\choiceIdsWithPoints ->
                            choiceIdsWithPoints
                                |> List.sortBy (Tuple.second >> negate)
                                |> List.head
                        )
            )


questBestChoicePoints : QuestId -> Query Int
questBestChoicePoints questId =
    questBestChoice questId
        |> Query.map (Maybe.map Tuple.second >> Maybe.withDefault 0)


questCompleted : QuestId -> Query Bool
questCompleted questId =
    Query.map2 (\threshold points -> points >= threshold)
        (questThreshold questId)
        (questBestChoicePoints questId)


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



-- CHOICE POINTS


choicePoints : ChoiceId -> Query (Dict ClientId Int)
choicePoints choiceId =
    Query.toplevel .choicePoints
        |> Query.andThen (Query.dictGet choiceId)


choiceTotalPoints : ChoiceId -> Query Int
choiceTotalPoints choiceId =
    choicePoints choiceId
        |> Query.map (Dict.foldl (\_ points acc -> acc + points) 0)


choiceClientPoints : ChoiceId -> Query Int
choiceClientPoints choiceId =
    Query.map2
        (\pointsDict clientId ->
            Dict.get clientId pointsDict
                |> Maybe.withDefault 0
        )
        (choicePoints choiceId)
        Query.clientId
