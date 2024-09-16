# lamdera-graphqlike

TL;DR: Automate sending of ToFrontend messages with current data whenever that data changes.

Queries will run after backend model changes (you can be more specific), and their results will be sent to frontend if they changed from last time.

```elm
-- You specify what data the frontend wants to know about:
subscriptions : List DataSub
subscriptions =
    [ userCountSub
    , leaderboardSub
    ]

-- The subscriptions look like this:
leaderboardSub : DataSub
leaderboardSub =
    Graphqlike.Sub.query
        "leaderboard" -- Cache key. We are only sending data to frontend if it has changed from the last time
        GotLeaderboard -- ToFrontend msg to send data with
        leaderboard -- The query to run
        -- The following are optimizations: you can skip computing the query if you're reasonably sure a msg can't affect it
        |> Graphqlike.Sub.fireOnlyAfterSpecificBackendMsgs
            (\msg ->
                case msg of
                    ClientConnected _ _ ->
                        False

                    ClientDisconnected _ _ ->
                        False

                    SaveToGraphqlikeCache _ _ ->
                        False
            )
        |> Graphqlike.Sub.fireOnlyAfterSpecificToBackendMsgs
            (\msg ->
                case msg of
                    Increment ->
                        True
            )

-- You can think of queries as `type alias Query a = BackendModel -> a`
-- (in reality they're more complex: they can fail, they know the current client ID, etc.)
-- They look like this:
leaderboard : Query (List (ClientId, Int))
leaderboard =
    Query.toplevel .messages -- reach for a BackendModel field
        |> Query.map -- do something to it! There's also andThen, andMap etc. all available.
            (\messages ->
                messages
                    |> List.map (\msg -> msg.clientId)
                    |> Dict.Extra.frequencies
                    |> Dict.toList
                    |> List.sortBy Tuple.second
                    |> List.take 5
            )
```

## Example

The machinery all lives inside `lib/`.

For a simple usage example look at the `src/` directory. It's a kind of a multiplayer Counter example, with an online user count and leaderboard tracked automatically. (Check out the lack of leaderboard derivation+sending in the `updateFromFrontend` `Increment` branch!)

For a more involved example look at the `example-game/` directory, though for it to actually run (with `lamdera live`), you'll probably have to replace the contents of `src/` with it.
