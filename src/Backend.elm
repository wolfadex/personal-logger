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


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { message = "Hello!" }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        LogsLoadResponse sessionId response ->
            ( model
            , response
                |> Result.mapError (\_ -> "Failed to load logs")
                |> TF_LogsLoaded
                |> Lamdera.sendToFrontend sessionId
            )

        MoreLogsLoadResponse sessionId response ->
            ( model
            , response
                |> Result.mapError (\_ -> "Failed to load logs")
                |> TF_MoreLogsLoaded
                |> Lamdera.sendToFrontend sessionId
            )

        CreateNewLog sessionId details isFirstLog log now ->
            ( model
            , githubRequest
                { method = "PUT"
                , token = Just details.token
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
                , expect =
                    Http.expectJson
                        (LogCreated sessionId { date = now, title = log.title, content = log.content })
                        (Json.Decode.at [ "content", "sha" ] Json.Decode.string)
                }
            )

        LogCreated sessionId log (Err _) ->
            ( model
            , Lamdera.sendToFrontend sessionId (TF_InsertLog log (Err "Failed to store log"))
            )

        LogCreated sessionId log (Ok sha) ->
            ( model
            , Lamdera.sendToFrontend sessionId (TF_InsertLog log (Ok sha))
            )

        StoreExistingChange sessionId details log now ->
            ( model
            , githubRequest
                { method = "PUT"
                , token = Just details.token
                , path = "/repos/" ++ details.owner ++ "/" ++ details.repo ++ "/contents/logs/" ++ String.fromInt (Time.posixToMillis log.timestamp) ++ ".json"
                , body =
                    [ ( "message", Json.Encode.string "Log edit" )
                    , ( "sha", Json.Encode.string log.sha )
                    , ( "content"
                      , ({ date = now
                         , title =
                            case (submittableValue log.currentLog).title of
                                Unmodified v ->
                                    v

                                Modified { modified } ->
                                    modified
                         , content =
                            case (submittableValue log.currentLog).content of
                                Unmodified v ->
                                    v

                                Modified { modified } ->
                                    modified
                         }
                            :: { date = (submittableValue log.currentLog).date
                               , title =
                                    case (submittableValue log.currentLog).title of
                                        Unmodified v ->
                                            v

                                        Modified { original } ->
                                            original
                               , content =
                                    case (submittableValue log.currentLog).content of
                                        Unmodified v ->
                                            v

                                        Modified { original } ->
                                            original
                               }
                            :: log.logHistory
                        )
                            |> Json.Encode.list
                                (\l ->
                                    [ ( "date", Json.Encode.int (Time.posixToMillis l.date) )
                                    , ( "title", Json.Encode.string l.title )
                                    , ( "content", Json.Encode.string l.content )
                                    ]
                                        |> Json.Encode.object
                                )
                            |> Json.Encode.encode 2
                            |> Base64.encode
                            |> Json.Encode.string
                      )
                    ]
                        |> Json.Encode.object
                        |> Http.jsonBody
                , expect =
                    Http.expectWhatever
                        (LogStored sessionId
                            ( ( log.timestamp, log.sha )
                            , { date = now
                              , title =
                                    Unmodified <|
                                        case (submittableValue log.currentLog).title of
                                            Unmodified v ->
                                                v

                                            Modified { modified } ->
                                                modified
                              , content =
                                    Unmodified <|
                                        case (submittableValue log.currentLog).content of
                                            Unmodified v ->
                                                v

                                            Modified { modified } ->
                                                modified
                              }
                            , { date = (submittableValue log.currentLog).date
                              , title =
                                    case (submittableValue log.currentLog).title of
                                        Unmodified v ->
                                            v

                                        Modified { original } ->
                                            original
                              , content =
                                    case (submittableValue log.currentLog).content of
                                        Unmodified v ->
                                            v

                                        Modified { original } ->
                                            original
                              }
                                :: log.logHistory
                            )
                        )
                }
            )

        LogStored sessionId ( ( timestamp, sha ), newLog, logHistory ) (Err _) ->
            ( model
            , Lamdera.sendToFrontend sessionId
                (TF_LogUpdated
                    { timestamp = timestamp, currentLog = Issue newLog "Failed to saved log changes", logHistory = logHistory, sha = sha }
                    (Err "Failed to saved log changes")
                )
            )

        LogStored sessionId ( ( timestamp, sha ), newLog, logHistory ) (Ok ()) ->
            ( model
            , Lamdera.sendToFrontend sessionId
                (TF_LogUpdated
                    { timestamp = timestamp, currentLog = Fresh newLog, logHistory = logHistory, sha = sha }
                    (Ok ())
                )
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId _ msg model =
    case msg of
        TB_LoadLogs details ->
            let
                owner =
                    case details of
                        StorageWrite writeDetails ->
                            writeDetails.owner

                        StorageRead readDetails ->
                            readDetails.owner

                repo =
                    case details of
                        StorageWrite writeDetails ->
                            writeDetails.repo

                        StorageRead readDetails ->
                            readDetails.repo
            in
            ( model
            , githubRequestTask
                { method = "GET"
                , token =
                    case details of
                        StorageWrite writeDetails ->
                            Just writeDetails.token

                        StorageRead _ ->
                            Nothing
                , path = "/repos/" ++ owner ++ "/" ++ repo ++ "/contents/logs"
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
                                                (Json.Decode.map3 (\name downloadUrl sha -> ( name, downloadUrl, sha ))
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
                                                    (Json.Decode.field "sha" Json.Decode.string)
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
                                (\( name, downloadUrl, sha ) ->
                                    Http.task
                                        { method = "GET"
                                        , headers =
                                            List.filterMap identity
                                                [ Just <| Http.header "Accept" "application/vnd.github.object+json"
                                                , Just <| Http.header "X-GitHub-Api-Version" "2022-11-28"
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
                                                                    Ok ( name, value, sha )

                                                                Err err ->
                                                                    Err (Http.BadBody (Json.Decode.errorToString err))
                                                )
                                        , timeout = Nothing
                                        }
                                )
                            |> Task.sequence
                            |> Task.map (\loadedLogs -> ( loadedLogs, List.drop 5 revList ))
                    )
                |> Task.map
                    (\( loaded, unloaded ) ->
                        ( List.map
                            (\( time, ( log, logHistory ), sha ) ->
                                { timestamp = time, currentLog = log, logHistory = logHistory, sha = sha }
                            )
                            loaded
                        , List.map
                            (\( timestamp, downloadUrl, sha ) ->
                                { timestamp = timestamp, downloadUrl = downloadUrl, sha = sha }
                            )
                            unloaded
                        )
                    )
                |> Task.attempt (LogsLoadResponse sessionId)
            )

        TB_CreateNewLog details isFirstLog log ->
            ( model
            , Time.now
                |> Task.perform (CreateNewLog sessionId details isFirstLog log)
            )

        TB_LoadMoreLogs logsToLoad ->
            ( model
            , logsToLoad
                |> List.map
                    (\toLoad ->
                        Http.task
                            { method = "GET"
                            , headers =
                                List.filterMap identity
                                    [ Just <| Http.header "Accept" "application/vnd.github.object+json"
                                    , Just <| Http.header "X-GitHub-Api-Version" "2022-11-28"
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
                                proxy ++ toLoad.downloadUrl
                            , body = Http.emptyBody
                            , resolver =
                                Http.stringResolver
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
                                                        Ok ( toLoad.timestamp, value, toLoad.sha )

                                                    Err err ->
                                                        Err (Http.BadBody (Json.Decode.errorToString err))
                                    )
                            , timeout = Nothing
                            }
                    )
                |> Task.sequence
                |> Task.map (List.map (\( timestamp, ( log, logHistory ), sha ) -> { timestamp = timestamp, currentLog = log, logHistory = logHistory, sha = sha }))
                |> Task.attempt (MoreLogsLoadResponse sessionId)
            )

        TB_SubmitExistingChange details log ->
            ( model
            , Time.now
                |> Task.perform (StoreExistingChange sessionId details log)
            )


decodeLog : Json.Decode.Decoder ( Submittable EditableLog, List Log )
decodeLog =
    Json.Decode.list
        (Json.Decode.map3
            (\date title content ->
                { date = Time.millisToPosix date
                , title = title
                , content = content
                }
            )
            (Json.Decode.field "date" Json.Decode.int)
            (Json.Decode.field "title" Json.Decode.string)
            (Json.Decode.field "content" Json.Decode.string)
        )
        |> Json.Decode.andThen
            (\logs ->
                case logs of
                    [] ->
                        Json.Decode.fail "Empty logs"

                    first :: rest ->
                        Json.Decode.succeed
                            ( Fresh
                                { date = first.date
                                , title = Unmodified first.title
                                , content = Unmodified first.content
                                }
                            , rest
                            )
            )


githubRequest :
    { body : Http.Body
    , expect : Http.Expect BackendMsg
    , method : String
    , path : String
    , token : Maybe String
    }
    -> Cmd BackendMsg
githubRequest options =
    Http.request
        { method = options.method
        , headers = toHeaders options.token
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
    , token : Maybe String
    }
    -> Task Http.Error a
githubRequestTask options =
    Http.task
        { method = options.method
        , headers = toHeaders options.token
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


toHeaders : Maybe String -> List Http.Header
toHeaders possibleToken =
    List.filterMap identity
        [ Just <| Http.header "Accept" "application/vnd.github+json"
        , case possibleToken of
            Just token ->
                Just <| Http.header "Authorization" ("Bearer " ++ token)

            Nothing ->
                Nothing
        , Just <| Http.header "X-GitHub-Api-Version" "2022-11-28"
        ]
