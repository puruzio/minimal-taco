module Types exposing (..)

import Time exposing (Time)

type alias Taco =
    {currentTime : Time
    , logdata : LogData
}

type alias LogData =
    List Log

type alias Messages =
    List String

type alias ChatChannel =
    String

type alias Flags =
    {
     currentTime : Time
    }

type TacoUpdate
    =NoUpdate
    | UpdateTime Time
    | UpdateLogData LogData


type alias Log =
    { id : String
    , text : String
    , created_at : String
    }

