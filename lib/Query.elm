module Query exposing
    ( Query
    , andMap
    , andThen
    , andThen2
    , clientId
    , dictGet
    , fail
    , map
    , map2
    , succeed
    , toplevel
    , traverse
    )

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Query.Error exposing (Error)


type alias Query m a =
    -- TODO maybe make this opaque?
    ClientId -> m -> Result Error a


succeed : a -> Query m a
succeed a =
    \_ _ -> Ok a


fail : Error -> Query m a
fail e =
    \_ _ -> Err e


map : (a -> b) -> Query m a -> Query m b
map f query =
    \clientId_ model ->
        query clientId_ model
            |> Result.map f


map2 : (a -> b -> c) -> Query m a -> Query m b -> Query m c
map2 f query1 query2 =
    \clientId_ model ->
        Result.map2 f
            (query1 clientId_ model)
            (query2 clientId_ model)


andMap : Query m a -> Query m (a -> b) -> Query m b
andMap queryPart queryConstructor =
    \clientId_ model ->
        Result.map2 (\a f -> f a)
            (queryPart clientId_ model)
            (queryConstructor clientId_ model)


andThen : (a -> Query m b) -> Query m a -> Query m b
andThen f query =
    \clientId_ model ->
        query clientId_ model
            |> Result.andThen
                (\a ->
                    let
                        innerQuery =
                            f a
                    in
                    innerQuery clientId_ model
                )


andThen2 : (a -> b -> Query m c) -> Query m a -> Query m b -> Query m c
andThen2 f query1 query2 =
    map2 f query1 query2
        |> andThen identity


traverse : (a -> Query m b) -> List a -> Query m (List b)
traverse f list =
    \clientId_ model ->
        List.foldr
            (\a acc ->
                let
                    innerQuery =
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
    \_ _ ->
        Dict.get key dict
            |> Result.fromMaybe (Query.Error.NotFound { dictKey = key })


toplevel : (m -> a) -> Query m a
toplevel f =
    \_ model -> Ok (f model)


clientId : Query m ClientId
clientId =
    \clientId_ _ -> Ok clientId_
