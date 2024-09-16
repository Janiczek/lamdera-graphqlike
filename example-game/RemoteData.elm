module RemoteData exposing (RemoteData(..))

import Query.Error


type RemoteData a
    = Loading
    | Error Query.Error.Error
    | Success a
