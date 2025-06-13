module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode
import Json.Encode
import Lamdera
import LocalStorage
import PkgPorts
import Time
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , logs = Unloaded
      , newLog = Fresh { title = "", content = "" }
      , owner = Untouched ""
      , repo = Untouched ""
      , token = Untouched ""
      , settingsOpen = False
      , theme = NoTheme
      }
    , Cmd.batch
        [ LocalStorage.get PkgPorts.ports "theme"
        , LocalStorage.get PkgPorts.ports "owner"
        , LocalStorage.get PkgPorts.ports "repo"
        ]
    )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    LocalStorage.gotten PkgPorts.ports LocalStorageKeyReceived


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        LocalStorageKeyReceived ( "theme", Nothing ) ->
            ( model, Cmd.none )

        LocalStorageKeyReceived ( "theme", Just value ) ->
            ( { model
                | theme =
                    case value of
                        "none" ->
                            NoTheme

                        "1" ->
                            Theme_1

                        "2" ->
                            Theme_2

                        "3" ->
                            Theme_3

                        "4" ->
                            Theme_4

                        "5" ->
                            Theme_5

                        "6" ->
                            Theme_6

                        _ ->
                            model.theme
              }
            , Cmd.none
            )

        LocalStorageKeyReceived ( "owner", Nothing ) ->
            ( model, Cmd.none )

        LocalStorageKeyReceived ( "owner", Just "" ) ->
            ( model, Cmd.none )

        LocalStorageKeyReceived ( "owner", Just owner ) ->
            ( { model | owner = Committed owner (Ok owner) }, Cmd.none )

        LocalStorageKeyReceived ( "repo", Nothing ) ->
            ( model, Cmd.none )

        LocalStorageKeyReceived ( "repo", Just "" ) ->
            ( model, Cmd.none )

        LocalStorageKeyReceived ( "repo", Just repo ) ->
            ( { model | repo = Committed repo (Ok repo) }, Cmd.none )

        LocalStorageKeyReceived _ ->
            ( model, Cmd.none )

        UserClickedSettingsOpen ->
            ( { model | settingsOpen = True }, Cmd.none )

        UserClickedSettingsClose ->
            ( { model | settingsOpen = False }, Cmd.none )

        SetTheme theme ->
            ( { model | theme = theme }
            , LocalStorage.set PkgPorts.ports "theme" <|
                case theme of
                    NoTheme ->
                        "none"

                    Theme_1 ->
                        "1"

                    Theme_2 ->
                        "2"

                    Theme_3 ->
                        "3"

                    Theme_4 ->
                        "4"

                    Theme_5 ->
                        "5"

                    Theme_6 ->
                        "6"
            )

        OwnerChanged owner ->
            ( { model | owner = Editing owner }, Cmd.none )

        OwnerBlurred ->
            let
                nextOwner =
                    commitField
                        (\owner ->
                            if String.isEmpty owner then
                                Err "Owner is required"

                            else
                                Ok owner
                        )
                        model.owner
            in
            ( { model
                | owner = nextOwner
              }
            , case nextOwner of
                Committed _ (Ok owner) ->
                    LocalStorage.set PkgPorts.ports "owner" owner

                _ ->
                    Cmd.none
            )

        RepoChanged repo ->
            ( { model | repo = Editing repo }, Cmd.none )

        RepoBlurred ->
            let
                nextRepo =
                    commitField
                        (\repo ->
                            if String.isEmpty repo then
                                Err "Repo is required"

                            else
                                Ok repo
                        )
                        model.repo
            in
            ( { model
                | repo = nextRepo
              }
            , case nextRepo of
                Committed _ (Ok repo) ->
                    LocalStorage.set PkgPorts.ports "repo" repo

                _ ->
                    Cmd.none
            )

        TokenChanged token ->
            ( { model | token = Editing token }, Cmd.none )

        TokenBlurred ->
            ( { model
                | token =
                    commitField
                        (\token ->
                            if String.isEmpty token then
                                Err "Token is required"

                            else
                                Ok token
                        )
                        model.token
              }
            , Cmd.none
            )

        LoadLogs ->
            case toStorageDetails model of
                Just storageDetails ->
                    ( { model | logs = Loading Nothing }
                    , Lamdera.sendToBackend (TB_LoadLogs storageDetails)
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        CreateLog ->
            case model.newLog of
                Submitting _ ->
                    ( model, Cmd.none )

                Fresh newLog ->
                    ( { model
                        | newLog =
                            Submitting newLog
                      }
                    , case toStorageDetails model of
                        Just storageDetails ->
                            { title = newLog.title, content = newLog.content }
                                |> TB_CreateNewLog storageDetails (model.logs == Loaded ( [], [] ))
                                |> Lamdera.sendToBackend

                        Nothing ->
                            Cmd.none
                    )

                Issue newLog _ ->
                    ( { model
                        | newLog =
                            Submitting newLog
                      }
                    , case toStorageDetails model of
                        Just storageDetails ->
                            { title = newLog.title, content = newLog.content }
                                |> TB_CreateNewLog storageDetails (model.logs == Loaded ( [], [] ))
                                |> Lamdera.sendToBackend

                        Nothing ->
                            Cmd.none
                    )

        NewLogTitleChanged title ->
            case model.newLog of
                Submitting _ ->
                    ( model, Cmd.none )

                Issue newLog _ ->
                    ( { model | newLog = Fresh { newLog | title = title } }, Cmd.none )

                Fresh newLog ->
                    ( { model | newLog = Fresh { newLog | title = title } }, Cmd.none )

        NewLogContentChanged content ->
            case model.newLog of
                Submitting _ ->
                    ( model, Cmd.none )

                Issue newLog _ ->
                    ( { model | newLog = Fresh { newLog | content = content } }, Cmd.none )

                Fresh newLog ->
                    ( { model | newLog = Fresh { newLog | content = content } }, Cmd.none )

        ExistingLogTitleChanged timestamp title ->
            ( { model
                | logs =
                    case model.logs of
                        Loaded ( logs, unloadedLogs ) ->
                            Loaded
                                ( List.map
                                    (\log ->
                                        if log.timestamp == timestamp then
                                            { log
                                                | currentLog =
                                                    case log.currentLog of
                                                        Submitting _ ->
                                                            log.currentLog

                                                        Fresh l ->
                                                            Fresh
                                                                { l
                                                                    | title =
                                                                        case l.title of
                                                                            Unmodified title_ ->
                                                                                if title_ == title then
                                                                                    l.title

                                                                                else
                                                                                    Modified { original = title_, modified = title }

                                                                            Modified modified ->
                                                                                if modified.original == title then
                                                                                    Unmodified modified.original

                                                                                else
                                                                                    Modified { modified | modified = title }
                                                                }

                                                        Issue l _ ->
                                                            Fresh
                                                                { l
                                                                    | title =
                                                                        case l.title of
                                                                            Unmodified title_ ->
                                                                                if title_ == title then
                                                                                    l.title

                                                                                else
                                                                                    Modified { original = title_, modified = title }

                                                                            Modified modified ->
                                                                                if modified.original == title then
                                                                                    Unmodified modified.original

                                                                                else
                                                                                    Modified { modified | modified = title }
                                                                }
                                            }

                                        else
                                            log
                                    )
                                    logs
                                , unloadedLogs
                                )

                        _ ->
                            model.logs
              }
            , Cmd.none
            )

        ExistingLogContentChanged timestamp content ->
            ( { model
                | logs =
                    case model.logs of
                        Loaded ( logs, unloadedLogs ) ->
                            Loaded
                                ( List.map
                                    (\log ->
                                        if log.timestamp == timestamp then
                                            { log
                                                | currentLog =
                                                    case log.currentLog of
                                                        Submitting _ ->
                                                            log.currentLog

                                                        Fresh l ->
                                                            Fresh
                                                                { l
                                                                    | content =
                                                                        case l.content of
                                                                            Unmodified content_ ->
                                                                                if content_ == content then
                                                                                    l.content

                                                                                else
                                                                                    Modified { original = content_, modified = content }

                                                                            Modified modified ->
                                                                                if modified.original == content then
                                                                                    Unmodified modified.original

                                                                                else
                                                                                    Modified { modified | modified = content }
                                                                }

                                                        Issue l _ ->
                                                            Fresh
                                                                { l
                                                                    | content =
                                                                        case l.content of
                                                                            Unmodified content_ ->
                                                                                if content_ == content then
                                                                                    l.content

                                                                                else
                                                                                    Modified { original = content_, modified = content }

                                                                            Modified modified ->
                                                                                if modified.original == content then
                                                                                    Unmodified modified.original

                                                                                else
                                                                                    Modified { modified | modified = content }
                                                                }
                                            }

                                        else
                                            log
                                    )
                                    logs
                                , unloadedLogs
                                )

                        _ ->
                            model.logs
              }
            , Cmd.none
            )

        SubmitExistingChange timestamp ->
            let
                submitChanges : (( List EditableLogList, b ) -> LogResult) -> ( List EditableLogList, b ) -> ( FrontendModel, Cmd frontendMsg )
                submitChanges toModelLogs ( loadedLogs, unloadedLogs ) =
                    case findLogToStore timestamp loadedLogs of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ( loadedLogsSubmitting, logToChange ) ->
                            case toStorageDetails model of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just storageDetails ->
                                    ( { model | logs = toModelLogs ( loadedLogsSubmitting, unloadedLogs ) }
                                    , logToChange
                                        |> TB_SubmitExistingChange storageDetails
                                        |> Lamdera.sendToBackend
                                    )
            in
            case model.logs of
                Unloaded ->
                    ( model, Cmd.none )

                Loaded logs ->
                    submitChanges Loaded logs

                Loading Nothing ->
                    ( model, Cmd.none )

                Loading (Just logs) ->
                    submitChanges (\l -> Loading (Just l)) logs

                Failure Nothing _ ->
                    ( model, Cmd.none )

                Failure (Just logs) err ->
                    submitChanges (\l -> Failure (Just l) err) logs

        LoadMoreLogs ->
            ( { model
                | logs =
                    case model.logs of
                        Unloaded ->
                            Loading Nothing

                        Loaded logs ->
                            Loading (Just logs)

                        Loading _ ->
                            model.logs

                        Failure loaded _ ->
                            Loading loaded
              }
            , case model.logs of
                Unloaded ->
                    case toStorageDetails model of
                        Just storageDetails ->
                            storageDetails
                                |> TB_LoadLogs
                                |> Lamdera.sendToBackend

                        Nothing ->
                            Cmd.none

                Loaded ( _, unloaded ) ->
                    case toStorageDetails model of
                        Just storageDetails ->
                            unloaded
                                |> List.take 5
                                |> TB_LoadMoreLogs storageDetails
                                |> Lamdera.sendToBackend

                        Nothing ->
                            Cmd.none

                Loading _ ->
                    Cmd.none

                Failure Nothing _ ->
                    case toStorageDetails model of
                        Just storageDetails ->
                            storageDetails
                                |> TB_LoadLogs
                                |> Lamdera.sendToBackend

                        Nothing ->
                            Cmd.none

                Failure (Just ( _, unloaded )) _ ->
                    case toStorageDetails model of
                        Just storageDetails ->
                            unloaded
                                |> List.take 5
                                |> TB_LoadMoreLogs storageDetails
                                |> Lamdera.sendToBackend

                        Nothing ->
                            Cmd.none
            )


findLogToStore : Time.Posix -> List EditableLogList -> Maybe ( List EditableLogList, EditableLogList )
findLogToStore timestamp logs =
    findLogToStoreHelper timestamp [] logs


findLogToStoreHelper : Time.Posix -> List EditableLogList -> List EditableLogList -> Maybe ( List EditableLogList, EditableLogList )
findLogToStoreHelper timestamp searchedLogs logs =
    case logs of
        [] ->
            Nothing

        log :: rest ->
            if timestamp == log.timestamp then
                Just ( List.reverse searchedLogs ++ { log | currentLog = toSubmittable log.currentLog } :: rest, log )

            else
                findLogToStore timestamp rest


commitField : (raw -> Result String parsed) -> Field raw parsed -> Field raw parsed
commitField parse field =
    case field of
        Untouched raw ->
            Committed raw (parse raw)

        Editing raw ->
            Committed raw (parse raw)

        Committed raw _ ->
            Committed raw (parse raw)


fieldRaw : Field raw parsed -> raw
fieldRaw field =
    case field of
        Untouched raw ->
            raw

        Editing raw ->
            raw

        Committed raw _ ->
            raw


fieldError : Field raw parsed -> Maybe String
fieldError field =
    case field of
        Untouched _ ->
            Nothing

        Editing _ ->
            Nothing

        Committed _ (Ok _) ->
            Nothing

        Committed _ (Err err) ->
            Just err


toStorageDetails : FrontendModel -> Maybe StorageDetails
toStorageDetails model =
    case ( model.owner, model.repo, model.token ) of
        ( Committed _ (Ok owner), Committed _ (Ok repo), Committed _ (Ok token) ) ->
            Just
                { owner = owner
                , repo = repo
                , token = token
                }

        _ ->
            Nothing


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TF_LogsLoaded response ->
            ( { model
                | logs =
                    case response of
                        Ok logs ->
                            Loaded logs

                        Err err ->
                            case model.logs of
                                Unloaded ->
                                    Failure Nothing err

                                Loading logs ->
                                    Failure logs err

                                Loaded logs ->
                                    Failure (Just logs) err

                                Failure logs _ ->
                                    Failure logs err
              }
            , Cmd.none
            )

        TF_MoreLogsLoaded response ->
            ( { model
                | logs =
                    case response of
                        Ok newLogs ->
                            case model.logs of
                                Unloaded ->
                                    Loaded ( newLogs, [] )

                                Loading Nothing ->
                                    Loaded ( newLogs, [] )

                                Loading (Just logs) ->
                                    newLogs
                                        |> List.foldl upsertLog logs
                                        |> Loaded

                                Loaded logs ->
                                    Loaded logs

                                Failure Nothing _ ->
                                    Loaded ( newLogs, [] )

                                Failure (Just logs) _ ->
                                    newLogs
                                        |> List.foldl upsertLog logs
                                        |> Loaded

                        Err err ->
                            case model.logs of
                                Unloaded ->
                                    Failure Nothing err

                                Loading logs ->
                                    Failure logs err

                                Loaded logs ->
                                    Failure (Just logs) err

                                Failure logs _ ->
                                    Failure logs err
              }
            , Cmd.none
            )

        TF_InsertLog _ (Err err) ->
            ( { model
                | newLog = submittableWithError err model.newLog
              }
            , Cmd.none
            )

        TF_InsertLog newLog (Ok sha) ->
            ( { model
                | newLog = Fresh { title = "", content = "" }
                , logs =
                    case model.logs of
                        Loaded logs ->
                            insertLog sha newLog logs
                                |> Loaded

                        Failure (Just logs) err ->
                            Failure (Just (insertLog sha newLog logs)) err

                        Loading (Just logs) ->
                            Loading (Just (insertLog sha newLog logs))

                        _ ->
                            model.logs
              }
            , Cmd.none
            )

        TF_LogUpdated log (Err _) ->
            ( { model
                | logs =
                    case model.logs of
                        Loaded logs ->
                            upsertLog log logs
                                |> Loaded

                        Failure (Just logs) err ->
                            Failure (Just (upsertLog log logs)) err

                        Loading (Just logs) ->
                            Loading (Just (upsertLog log logs))

                        _ ->
                            model.logs
              }
            , Cmd.none
            )

        TF_LogUpdated log (Ok ()) ->
            ( { model
                | logs =
                    case model.logs of
                        Loaded logs ->
                            upsertLog log logs
                                |> Loaded

                        Failure (Just logs) err ->
                            Failure (Just (upsertLog log logs)) err

                        Loading (Just logs) ->
                            Loading (Just (upsertLog log logs))

                        _ ->
                            model.logs
              }
            , Cmd.none
            )


upsertLog : EditableLogList -> LoadedLogs -> LoadedLogs
upsertLog newLog ( logs, unloadedLogs ) =
    ( case logs of
        [] ->
            [ newLog ]

        recentLog :: rest ->
            upsetLogHelper newLog [] recentLog rest
    , List.filter (\unloaded -> unloaded.timestamp == newLog.timestamp) unloadedLogs
    )


upsetLogHelper : EditableLogList -> List EditableLogList -> EditableLogList -> List EditableLogList -> List EditableLogList
upsetLogHelper newLog checkedLogs recentLog toCheckLogs =
    if Time.posixToMillis newLog.timestamp == Time.posixToMillis recentLog.timestamp then
        List.reverse checkedLogs ++ newLog :: toCheckLogs

    else if Time.posixToMillis newLog.timestamp > Time.posixToMillis recentLog.timestamp then
        List.reverse checkedLogs ++ newLog :: recentLog :: toCheckLogs

    else
        case toCheckLogs of
            [] ->
                List.reverse checkedLogs ++ [ recentLog, newLog ]

            nextMostRecent :: rest ->
                upsetLogHelper newLog (recentLog :: checkedLogs) nextMostRecent rest


toEditableLog : String -> Log -> EditableLogList
toEditableLog sha log =
    { timestamp = log.date
    , currentLog =
        Fresh
            { date = log.date
            , title = Unmodified log.title
            , content = Unmodified log.content
            }
    , logHistory = []
    , sha = sha
    }


insertLog : String -> Log -> LoadedLogs -> LoadedLogs
insertLog sha newLog ( logs, unloadedLogs ) =
    ( case logs of
        [] ->
            [ toEditableLog sha newLog ]

        recentLog :: rest ->
            insertLogHelper (toEditableLog sha newLog) [] recentLog rest
    , unloadedLogs
    )


insertLogHelper : EditableLogList -> List EditableLogList -> EditableLogList -> List EditableLogList -> List EditableLogList
insertLogHelper newLog checkedLogs recentLog toCheckLogs =
    if Time.posixToMillis newLog.timestamp > Time.posixToMillis recentLog.timestamp then
        List.reverse checkedLogs ++ newLog :: recentLog :: toCheckLogs

    else
        case toCheckLogs of
            [] ->
                List.reverse checkedLogs ++ [ recentLog, newLog ]

            nextMostRecent :: rest ->
                insertLogHelper newLog (recentLog :: checkedLogs) nextMostRecent rest


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Personal Logger"
    , body =
        [ Html.main_
            [ case model.theme of
                NoTheme ->
                    Html.Attributes.class ""

                Theme_1 ->
                    Css.theme_1

                Theme_2 ->
                    Css.theme_2

                Theme_3 ->
                    Css.theme_3

                Theme_4 ->
                    Css.theme_4

                Theme_5 ->
                    Css.theme_5

                Theme_6 ->
                    Css.theme_6
            ]
            [ Html.h1 [] [ Html.text "Personal Logger" ]
            , case model.logs of
                Unloaded ->
                    Html.button [ Html.Events.onClick LoadLogs ]
                        [ Html.text "Load logs" ]

                Loading Nothing ->
                    loader

                Loading (Just logs) ->
                    viewLogs model True logs

                Loaded logs ->
                    viewLogs model False logs

                Failure _ err ->
                    Html.text err
            ]
        , Html.button
            [ Css.settingsButton
            , Html.Events.onClick UserClickedSettingsOpen
            ]
            [ Html.text "Settings" ]
        , viewSettings model
        , Html.a
            [ Html.Attributes.href "https://ko-fi.com/D1D81GEXYM"
            , Html.Attributes.target "_blank"
            , Css.kofiButton
            ]
            [ Html.img
                [ Html.Attributes.height 36
                , Html.Attributes.style "border" "0px"
                , Html.Attributes.style "height" "36px"
                , Html.Attributes.src "https://storage.ko-fi.com/cdn/kofi6.png?v=6"
                , Html.Attributes.attribute "border" "0"
                , Html.Attributes.alt "Buy Me a Coffee at ko-fi.com"
                ]
                []
            ]
        ]
    }


viewSettings : FrontendModel -> Html FrontendMsg
viewSettings model =
    modal { isOpen = model.settingsOpen, onClose = UserClickedSettingsClose }
        [ Css.settingsModel ]
        [ Html.h2 [ Css.modalHeader ]
            [ Html.text "Settings"
            , Html.button
                [ Html.Attributes.attribute "aria-label" "Close"
                , Html.Attributes.rel "prev"
                , Html.Events.onClick UserClickedSettingsClose
                ]
                [ Html.text "Close" ]
            ]
        , Html.form [ Html.Events.onSubmit LoadLogs ]
            [ Html.label []
                [ Html.text "Owner"
                , Html.input
                    [ Html.Attributes.value <| fieldRaw model.owner
                    , Html.Events.onInput OwnerChanged
                    , Html.Events.onBlur OwnerBlurred
                    ]
                    []
                , case fieldError model.owner of
                    Nothing ->
                        Html.text ""

                    Just error ->
                        Html.span [ Css.inputError ] [ Html.text error ]
                ]
            , Html.label []
                [ Html.text "Repo"
                , Html.input
                    [ Html.Attributes.value <| fieldRaw model.repo
                    , Html.Events.onInput RepoChanged
                    , Html.Events.onBlur RepoBlurred
                    ]
                    []
                , case fieldError model.repo of
                    Nothing ->
                        Html.text ""

                    Just error ->
                        Html.span [ Css.inputError ] [ Html.text error ]
                ]
            , Html.label []
                [ Html.text "Token"
                , Html.input
                    [ Html.Attributes.type_ "password"
                    , Html.Attributes.value <| fieldRaw model.token
                    , Html.Events.onInput TokenChanged
                    , Html.Events.onBlur TokenBlurred
                    ]
                    []
                , case fieldError model.token of
                    Nothing ->
                        Html.text ""

                    Just error ->
                        Html.span [ Css.inputError ] [ Html.text error ]
                ]
            , Html.fieldset []
                [ Html.legend []
                    [ Html.text "Theme"
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "No theme" ]
                    , Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.name "theme"
                        , Html.Events.onInput (\_ -> SetTheme NoTheme)
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "Cubes" ]
                    , Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.name "theme"
                        , Html.Events.onInput (\_ -> SetTheme Theme_1)
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "Grid" ]
                    , Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.name "theme"
                        , Html.Events.onInput (\_ -> SetTheme Theme_2)
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "Dots dark" ]
                    , Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.name "theme"
                        , Html.Events.onInput (\_ -> SetTheme Theme_3)
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "Dots light" ]
                    , Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.name "theme"
                        , Html.Events.onInput (\_ -> SetTheme Theme_4)
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "Notebook" ]
                    , Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.name "theme"
                        , Html.Events.onInput (\_ -> SetTheme Theme_5)
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.span [] [ Html.text "Lines" ]
                    , Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.name "theme"
                        , Html.Events.onInput (\_ -> SetTheme Theme_6)
                        ]
                        []
                    ]
                ]
            ]
        ]


viewLogs : FrontendModel -> Bool -> LoadedLogs -> Html FrontendMsg
viewLogs model isLoading ( logs, unloadedLogs ) =
    let
        newLog =
            case model.newLog of
                Fresh l ->
                    l

                Issue l _ ->
                    l

                Submitting l ->
                    l

        newLogForm =
            Html.form [ Html.Events.onSubmit CreateLog ]
                [ Html.label []
                    [ Html.text "Title"
                    , Html.input
                        [ Html.Attributes.value newLog.title
                        , Html.Events.onInput NewLogTitleChanged
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.text "Content"
                    , Html.textarea
                        [ Html.Attributes.value newLog.content
                        , Html.Events.onInput NewLogContentChanged
                        ]
                        []
                    ]
                , case model.newLog of
                    Issue _ error ->
                        Html.span [ Css.inputError ] [ Html.text error ]

                    _ ->
                        Html.text ""
                , Html.button
                    [ Html.Attributes.type_ "submit" ]
                    [ Html.text "Store log"
                    , case model.newLog of
                        Submitting _ ->
                            loaderSimple

                        _ ->
                            Html.text ""
                    ]
                ]

        loadMoreButton =
            ( "load-more-button"
            , Html.button
                [ Html.Attributes.type_ "button"
                , Html.Events.onClick LoadMoreLogs
                ]
                [ Html.text "Load older logs" ]
            )

        loadingLogs =
            ( "log-loading-status"
            , if isLoading then
                loader

              else
                Html.text ""
            )
    in
    Html.Keyed.node "div" [ Css.logList ] <|
        case ( logs, unloadedLogs ) of
            ( [], [] ) ->
                [ ( "new-log-form", newLogForm ), loadingLogs ]

            ( [], _ ) ->
                [ loadingLogs, loadMoreButton ]

            ( _, [] ) ->
                logs
                    |> List.map viewLog
                    |> (\l -> l ++ [ loadingLogs ])
                    |> (::) ( "new-log-form", newLogForm )

            _ ->
                logs
                    |> List.map viewLog
                    |> (\l -> l ++ [ loadingLogs, loadMoreButton ])
                    |> (::) ( "new-log-form", newLogForm )


loader : Html msg
loader =
    Html.div [ Css.loader ] []


loaderSimple : Html msg
loaderSimple =
    Html.div [ Css.loaderSimple ] []


viewLog : EditableLogList -> ( String, Html FrontendMsg )
viewLog log =
    let
        currentVal modifiable =
            case modifiable of
                Unmodified val ->
                    val

                Modified { modified } ->
                    modified
    in
    ( log.timestamp |> Time.posixToMillis |> String.fromInt
    , Html.form
        [ Css.previousLog
        , Html.Events.onSubmit (SubmitExistingChange log.timestamp)
        ]
        [ Html.label [ Css.previousLogTitle ]
            [ Html.input
                [ Html.Attributes.value (currentVal (submittableValue log.currentLog).title)
                , Html.Attributes.placeholder "Title"
                , Html.Events.onInput (ExistingLogTitleChanged log.timestamp)
                ]
                []
            ]
        , Html.label [ Css.previousLogContent ]
            [ Html.textarea
                [ Html.Attributes.value (currentVal (submittableValue log.currentLog).content)
                , Html.Attributes.placeholder "Content"
                , Html.Events.onInput (ExistingLogContentChanged log.timestamp)
                ]
                []
            ]
        , case ( (submittableValue log.currentLog).title, (submittableValue log.currentLog).content ) of
            ( Unmodified _, Unmodified _ ) ->
                Html.text ""

            _ ->
                Html.button
                    [ Css.storeChangesButton
                    , Html.Attributes.type_ "submit"
                    ]
                    [ Html.text "Store changes"
                    , case log.currentLog of
                        Submitting _ ->
                            loaderSimple

                        _ ->
                            Html.text ""
                    ]
        ]
    )


modal : { isOpen : Bool, onClose : msg } -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
modal options attributes =
    Html.node "dialog"
        (Html.Attributes.property "___wolfadex_modal__open" (Json.Encode.bool (Debug.log "isOpen" options.isOpen))
            :: Html.Events.on "close" (Json.Decode.succeed options.onClose)
            :: attributes
        )
