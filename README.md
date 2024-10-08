# lamdera-graphqlike

> TL;DR: Automate sending of ToFrontend messages with current data whenever that data changes.

Queries will run after backend model changes (you can be more specific), and their results will be sent to frontend if they changed from last time.

```elm
-- You use our specific `main`
app =
    Graphqlike.backend
        { ...
        , dataSubscriptions = dataSubscriptions
        }

-- You specify what data the frontend wants to know about:
dataSubscriptions : List DataSub
dataSubscriptions =
    [ userCountSub
    , leaderboardSub
    ]

-- The subscriptions look like this:
leaderboardSub : DataSub
leaderboardSub =
    Graphqlike.Sub.query
        { cacheKey = "leaderboard" -- We're only sending data if it changed from last time
        , toMsg = GotLeaderboard
        , query = leaderboard
        }
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

## API Overview::

- `Graphqlike.backend`
    - a replacement for `Lamdera.backend`
- `Graphqlike.sendSubscriptionData`
    - a way to trigger the computation of queries and sending of data to frontends manually as a Cmd
    - handy for `ClientConnected` BackendMsg when we want to hydrate it for the first time.
- `Graphqlike.Sub.query`
    - a specification of what query to run, in reaction to what Msgs, and how to send the results to the frontend
- `Graphqlike.Sub.fireOnlyAfterSpecificBackendMsgs`
- `Graphqlike.Sub.fireOnlyAfterSpecificToBackendMsgs`
    - a way to skip computation of a query if you're sure some Msgs can't affect it
- `Query`
    - a computation (roughly `BackendModel -> a`)
    - all the usual applicative/monadic combinator goodies are present
    - can be ran with `Query.run`, although the `Graphqlike.backend` entrypoint will take care of that.
