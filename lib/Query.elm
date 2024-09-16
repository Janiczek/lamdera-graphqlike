module Query exposing
    ( Query, run
    , succeed
    , fail, notFound
    , toplevel, clientId
    , map, map2, andMap, andThen, andThen2, traverse
    , dictGet
    )

{-|

@docs Query, run
@docs succeed
@docs fail, notFound
@docs toplevel, clientId
@docs map, map2, andMap, andThen, andThen2, traverse
@docs dictGet

-}

import Dict exposing (Dict)
import Graphqlike.Internal exposing (Query(..))
import Lamdera exposing (ClientId)
import Query.Error exposing (Error)


type alias Query m a =
    Graphqlike.Internal.Query m a


run : ClientId -> m -> Query m a -> Result Error a
run clientId_ model (Q _ query) =
    -- TODO maybe there's a way to use phantom types or something similar to
    -- run clientId-using queries with clientId and those that don't, without
    -- it? Instead of making up a dummy clientId.
    query clientId_ model


succeed : a -> Query m a
succeed a =
    Q { usesClientId = False } <| \_ _ -> Ok a


fail : Error -> Query m a
fail e =
    Q { usesClientId = False } <| \_ _ -> Err e


notFound : String -> Query m a
notFound dictKey =
    fail (Query.Error.NotFound { dictKey = dictKey })


map : (a -> b) -> Query m a -> Query m b
map f (Q info query) =
    Q info <|
        \clientId_ model ->
            query clientId_ model
                |> Result.map f


map2 : (a -> b -> c) -> Query m a -> Query m b -> Query m c
map2 f (Q info1 query1) (Q info2 query2) =
    Q (combineInfo info1 info2) <|
        \clientId_ model ->
            Result.map2 f
                (query1 clientId_ model)
                (query2 clientId_ model)


andMap : Query m a -> Query m (a -> b) -> Query m b
andMap (Q info1 queryPart) (Q info2 queryConstructor) =
    Q (combineInfo info1 info2) <|
        \clientId_ model ->
            Result.map2 (\a f -> f a)
                (queryPart clientId_ model)
                (queryConstructor clientId_ model)


{-| This power unfortunately doesn't come for free - it turns off the "only run
a query once and broadcast across all clients" optimization. Use helpers other
than `andThen` if you can!
-}
andThen : (a -> Query m b) -> Query m a -> Query m b
andThen f (Q _ query) =
    -- At this point we can't know whether the inner query uses clientId or
    -- not, so let's be safe
    Q { usesClientId = True } <|
        \clientId_ model ->
            query clientId_ model
                |> Result.andThen
                    (\a ->
                        let
                            (Q _ innerQuery) =
                                f a
                        in
                        innerQuery clientId_ model
                    )


andThen2 : (a -> b -> Query m c) -> Query m a -> Query m b -> Query m c
andThen2 f query1 query2 =
    map2 f query1 query2
        |> andThen identity


{-| This power unfortunately doesn't come for free - it turns off the "only run
a query once and broadcast across all clients" optimization. Use helpers other
than `traverse` if you can!
-}
traverse : (a -> Query m b) -> List a -> Query m (List b)
traverse f list =
    -- Same story as with andThen.
    Q { usesClientId = True } <|
        \clientId_ model ->
            List.foldr
                (\a acc ->
                    let
                        (Q _ innerQuery) =
                            f a
                    in
                    case innerQuery clientId_ model of
                        Ok b ->
                            Result.map2 (::) (Ok b) acc

                        Err e ->
                            Err e
                )
                (Ok [])
                list


dictGet : String -> Dict String a -> Query m a
dictGet key dict =
    Q { usesClientId = False } <|
        \_ _ ->
            Dict.get key dict
                |> Result.fromMaybe (Query.Error.NotFound { dictKey = key })


toplevel : (m -> a) -> Query m a
toplevel f =
    Q { usesClientId = False } <| \_ model -> Ok (f model)


clientId : Query m ClientId
clientId =
    Q { usesClientId = True } <| \clientId_ _ -> Ok clientId_


combineInfo :
    { usesClientId : Bool }
    -> { usesClientId : Bool }
    -> { usesClientId : Bool }
combineInfo i1 i2 =
    { usesClientId = i1.usesClientId || i2.usesClientId }
