module Query.Error exposing (Error(..))


type Error
    = NotFound { dictKey : String }
