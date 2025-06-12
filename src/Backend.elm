module Backend exposing (..)

import Base64
import Env
import Http
import Json.Decode
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import Task exposing (Task)
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!" }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        LogsLoadResponse sessionId response ->
            ( model
            , response
                |> Result.mapError Debug.toString
                |> TF_LogsLoaded
                |> Lamdera.sendToFrontend sessionId
            )

        CreateNewLog sessionId details isFirstLog log now ->
            ( model
            , githubRequest
                { method = "PUT"
                , token = details.token
                , path = "/repos/" ++ details.owner ++ "/" ++ details.repo ++ "/contents/logs/" ++ String.fromInt (Time.posixToMillis now) ++ ".json"
                , body =
                    [ ( "message"
                      , Json.Encode.string
                            (if isFirstLog then
                                "First log"

                             else
                                "New log"
                            )
                      )
                    , ( "content"
                      , [ [ ( "date", Json.Encode.int (Time.posixToMillis now) )
                          , ( "title", Json.Encode.string log.title )
                          , ( "content", Json.Encode.string log.content )
                          ]
                            |> Json.Encode.object
                        ]
                            |> Json.Encode.list identity
                            |> Json.Encode.encode 2
                            |> Base64.encode
                            |> Json.Encode.string
                      )
                    ]
                        |> Json.Encode.object
                        |> Http.jsonBody
                , expect = Http.expectWhatever (LogCreated sessionId { date = now, title = log.title, content = log.content })
                }
            )

        LogCreated sessionId log (Err err) ->
            ( model, Cmd.none )

        LogCreated sessionId log (Ok ()) ->
            ( model
            , Lamdera.sendToFrontend sessionId (TF_InsertLog log)
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        TB_LoadLogs details ->
            ( model
            , githubRequestTask
                { method = "GET"
                , token = details.token
                , path = "/repos/" ++ details.owner ++ "/" ++ details.repo ++ "/contents/logs"
                , body = Http.emptyBody
                , resolver =
                    Http.stringResolver
                        -- (LogsLoadResponse sessionId)
                        (\response ->
                            case response of
                                Http.BadUrl_ url ->
                                    Err (Http.BadUrl url)

                                Http.Timeout_ ->
                                    Err Http.Timeout

                                Http.NetworkError_ ->
                                    Err Http.NetworkError

                                Http.BadStatus_ metadata body ->
                                    if metadata.statusCode == 404 then
                                        case Json.Decode.decodeString (Json.Decode.field "message" Json.Decode.string) body of
                                            Ok "Not Found" ->
                                                Ok []

                                            _ ->
                                                Err (Http.BadStatus metadata.statusCode)

                                    else
                                        Err (Http.BadStatus metadata.statusCode)

                                Http.GoodStatus_ _ body ->
                                    case
                                        Json.Decode.decodeString
                                            (Json.Decode.list
                                                (Json.Decode.map2 Tuple.pair
                                                    (Json.Decode.field "name" Json.Decode.string
                                                        |> Json.Decode.andThen
                                                            (\nameStr ->
                                                                case
                                                                    nameStr
                                                                        |> String.replace ".json" ""
                                                                        |> String.toInt
                                                                        |> Maybe.map Time.millisToPosix
                                                                of
                                                                    Nothing ->
                                                                        Json.Decode.fail "Expected a file name like <posix timestamp>.json"

                                                                    Just name ->
                                                                        Json.Decode.succeed name
                                                            )
                                                    )
                                                    (Json.Decode.field "download_url" Json.Decode.string)
                                                )
                                            )
                                            body
                                    of
                                        Ok value ->
                                            Ok value

                                        Err err ->
                                            Err (Http.BadBody (Json.Decode.errorToString err))
                        )
                }
                |> Task.andThen
                    (\logsByUrl ->
                        let
                            revList =
                                logsByUrl
                                    |> List.reverse
                        in
                        revList
                            |> List.take 5
                            |> List.map
                                (\( name, downloadUrl ) ->
                                    Http.task
                                        { method = "GET"
                                        , headers =
                                            [ Http.header "Accept" "application/vnd.github.object+json"
                                            , Http.header "Authorization" ("Bearer " ++ details.token)
                                            , Http.header "X-GitHub-Api-Version" "2022-11-28"
                                            ]
                                        , url =
                                            let
                                                proxy =
                                                    case Env.mode of
                                                        Env.Development ->
                                                            "http://localhost:8001/"

                                                        Env.Production ->
                                                            ""
                                            in
                                            proxy ++ downloadUrl
                                        , body = Http.emptyBody
                                        , resolver =
                                            Http.stringResolver
                                                -- (LogsLoadResponse sessionId)
                                                (\response ->
                                                    case response of
                                                        Http.BadUrl_ url ->
                                                            Err (Http.BadUrl url)

                                                        Http.Timeout_ ->
                                                            Err Http.Timeout

                                                        Http.NetworkError_ ->
                                                            Err Http.NetworkError

                                                        Http.BadStatus_ metadata _ ->
                                                            Err (Http.BadStatus metadata.statusCode)

                                                        Http.GoodStatus_ _ body ->
                                                            case Json.Decode.decodeString decodeLog body of
                                                                Ok value ->
                                                                    Ok ( name, value )

                                                                Err err ->
                                                                    Err (Http.BadBody (Json.Decode.errorToString err))
                                                )
                                        , timeout = Nothing
                                        }
                                )
                            |> Task.sequence
                            |> Task.map (\loadedLogs -> ( loadedLogs, List.drop 5 revList ))
                    )
                |> Task.attempt (LogsLoadResponse sessionId)
            )

        TB_CreateNewLog details isFirstLog log ->
            ( model
            , Time.now
                |> Task.perform (CreateNewLog sessionId details isFirstLog log)
            )


expectJsonCustomBadStatus : (Result Http.Error a -> msg) -> (Http.Metadata -> String -> Result Http.Error a) -> Json.Decode.Decoder a -> Http.Expect msg
expectJsonCustomBadStatus toMsg handleBadStatus decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    handleBadStatus metadata body

                Http.GoodStatus_ _ body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Json.Decode.errorToString err))


decodeLogs : Json.Decode.Decoder (List ( Time.Posix, NonEmptyList Log ))
decodeLogs =
    Json.Decode.list <|
        Json.Decode.map2 Tuple.pair
            (Json.Decode.field "name" Json.Decode.string
                |> Json.Decode.andThen
                    (\nameStr ->
                        case
                            nameStr
                                |> String.replace ".json" ""
                                |> String.toInt
                                |> Maybe.map Time.millisToPosix
                        of
                            Nothing ->
                                Json.Decode.fail "Expected a file name like <posix timestamp>.json"

                            Just name ->
                                Json.Decode.succeed name
                    )
            )
            (Json.Decode.field "content" Json.Decode.string
                |> Json.Decode.andThen
                    (\content ->
                        case Json.Decode.decodeString decodeLog content of
                            Ok log ->
                                Json.Decode.succeed log

                            Err err ->
                                Json.Decode.fail (Json.Decode.errorToString err)
                    )
            )


decodeLog : Json.Decode.Decoder (NonEmptyList Log)
decodeLog =
    Json.Decode.map3
        (\date title content ->
            { date = Time.millisToPosix date
            , title = title
            , content = content
            }
        )
        (Json.Decode.field "date" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
        |> Json.Decode.list
        |> Json.Decode.andThen
            (\logs ->
                case logs of
                    [] ->
                        Json.Decode.fail "Empty logs"

                    first :: rest ->
                        Json.Decode.succeed ( first, rest )
            )


githubRequest :
    { body : Http.Body
    , expect : Http.Expect BackendMsg
    , method : String
    , path : String
    , token : String
    }
    -> Cmd BackendMsg
githubRequest options =
    Http.request
        { method = options.method
        , headers =
            [ Http.header "Accept" "application/vnd.github+json"
            , Http.header "Authorization" ("Bearer " ++ options.token)
            , Http.header "X-GitHub-Api-Version" "2022-11-28"
            ]
        , url =
            let
                proxy =
                    case Env.mode of
                        Env.Development ->
                            "http://localhost:8001/"

                        Env.Production ->
                            ""
            in
            proxy ++ "https://api.github.com" ++ options.path
        , body = options.body
        , expect = options.expect
        , timeout = Nothing
        , tracker = Nothing
        }


githubRequestTask :
    { body : Http.Body
    , resolver : Http.Resolver Http.Error a
    , method : String
    , path : String
    , token : String
    }
    -> Task Http.Error a
githubRequestTask options =
    Http.task
        { method = options.method
        , headers =
            [ Http.header "Accept" "application/vnd.github+json"
            , Http.header "Authorization" ("Bearer " ++ options.token)
            , Http.header "X-GitHub-Api-Version" "2022-11-28"
            ]
        , url =
            let
                proxy =
                    case Env.mode of
                        Env.Development ->
                            "http://localhost:8001/"

                        Env.Production ->
                            ""
            in
            proxy ++ "https://api.github.com" ++ options.path
        , body = options.body
        , resolver = options.resolver
        , timeout = Nothing
        }
