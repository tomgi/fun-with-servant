module Generated exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias User =
    { firstName : String
    , lastName : String
    }

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "firstName", Json.Encode.string x.firstName )
        , ( "lastName", Json.Encode.string x.lastName )
        ]

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "firstName" string
        |> required "lastName" string

type alias DateTime =
    { time : String
    , date : String
    , milliseconds_since_epoch : Int
    }

decodeDateTime : Decoder DateTime
decodeDateTime =
    decode DateTime
        |> required "time" string
        |> required "date" string
        |> required "milliseconds_since_epoch" int

type alias IP =
    { ip : String
    }

decodeIP : Decoder IP
decodeIP =
    decode IP
        |> required "ip" string

type alias DateTimeAndIP =
    { dateTime : DateTime
    , ipAddress : IP
    }

decodeDateTimeAndIP : Decoder DateTimeAndIP
decodeDateTimeAndIP =
    decode DateTimeAndIP
        |> required "dateTime" decodeDateTime
        |> required "ipAddress" decodeIP

getApiUsers : Http.Request (List (User))
getApiUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postApiUsersCreate : User -> Http.Request (Int)
postApiUsersCreate body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "users"
                , "create"
                ]
        , body =
            Http.jsonBody (encodeUser body)
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiTime : Http.Request (DateTimeAndIP)
getApiTime =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "time"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeDateTimeAndIP
        , timeout =
            Nothing
        , withCredentials =
            False
        }