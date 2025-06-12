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
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , logs = Unloaded
      , newLog = Fresh { title = "", content = "" }
      , owner = ""
      , repo = ""
      , githubToken = ""
      , settingsOpen = False
      , theme = NoTheme
      }
    , Cmd.none
    )


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

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        UserClickedSettingsOpen ->
            ( { model | settingsOpen = True }, Cmd.none )

        UserClickedSettingsClose ->
            ( { model | settingsOpen = False }, Cmd.none )

        SetTheme theme ->
            ( { model | theme = theme }, Cmd.none )

        LoadLogs ->
            ( { model | logs = Loading Nothing }
            , Lamdera.sendToBackend (TB_LoadLogs (toStorageDetails model))
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
                    , Lamdera.sendToBackend
                        (TB_CreateNewLog (toStorageDetails model)
                            (model.logs == Loaded ( [], [] ))
                            { title = newLog.title, content = newLog.content }
                        )
                    )

                Issue newLog _ ->
                    ( { model
                        | newLog =
                            Submitting newLog
                      }
                    , Lamdera.sendToBackend
                        (TB_CreateNewLog (toStorageDetails model)
                            (model.logs == Loaded ( [], [] ))
                            { title = newLog.title, content = newLog.content }
                        )
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
                                    (\( t, ( log, rest ) ) ->
                                        if t == timestamp then
                                            ( t, ( { log | title = title }, rest ) )

                                        else
                                            ( t, ( log, rest ) )
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
                                    (\( t, ( log, rest ) ) ->
                                        if t == timestamp then
                                            ( t, ( { log | content = content }, rest ) )

                                        else
                                            ( t, ( log, rest ) )
                                    )
                                    logs
                                , unloadedLogs
                                )

                        _ ->
                            model.logs
              }
            , Cmd.none
            )

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
                    toStorageDetails model
                        |> TB_LoadLogs
                        |> Lamdera.sendToBackend

                Loaded ( _, unloaded ) ->
                    unloaded
                        |> List.take 5
                        |> TB_LoadMoreLogs (toStorageDetails model)
                        |> Lamdera.sendToBackend

                Loading _ ->
                    Cmd.none

                Failure Nothing _ ->
                    toStorageDetails model
                        |> TB_LoadLogs
                        |> Lamdera.sendToBackend

                Failure (Just ( _, unloaded )) _ ->
                    unloaded
                        |> List.take 5
                        |> TB_LoadMoreLogs (toStorageDetails model)
                        |> Lamdera.sendToBackend
            )


toStorageDetails : FrontendModel -> StorageDetails
toStorageDetails model =
    { owner = model.owner, repo = model.repo, token = model.githubToken }


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

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

        TF_InsertLog newLog ->
            ( { model
                | newLog = Fresh { title = "", content = "" }
                , logs =
                    case model.logs of
                        Loaded logs ->
                            insertLog newLog logs
                                |> Loaded

                        _ ->
                            model.logs
              }
            , Cmd.none
            )


upsertLog : ( Time.Posix, NonEmptyList Log ) -> LoadedLogs -> LoadedLogs
upsertLog (( newLogDate, _ ) as newLog) ( logs, unloadedLogs ) =
    ( case logs of
        [] ->
            [ newLog ]

        recentLog :: rest ->
            upsetLogHelper newLog [] recentLog rest
    , List.filter (\( unloadedDate, _ ) -> unloadedDate == newLogDate) unloadedLogs
    )


upsetLogHelper : ( Time.Posix, NonEmptyList Log ) -> List ( Time.Posix, NonEmptyList Log ) -> ( Time.Posix, NonEmptyList Log ) -> List ( Time.Posix, NonEmptyList Log ) -> List ( Time.Posix, NonEmptyList Log )
upsetLogHelper (( newLogDate, _ ) as newLog) checkedLogs ( recentLogTime, recentLog ) toCheckLogs =
    if Time.posixToMillis newLogDate == Time.posixToMillis recentLogTime then
        List.reverse checkedLogs ++ newLog :: toCheckLogs

    else if Time.posixToMillis newLogDate > Time.posixToMillis recentLogTime then
        List.reverse checkedLogs ++ newLog :: ( recentLogTime, recentLog ) :: toCheckLogs

    else
        case toCheckLogs of
            [] ->
                List.reverse checkedLogs ++ [ ( recentLogTime, recentLog ), newLog ]

            nextMostRecent :: rest ->
                upsetLogHelper newLog (( recentLogTime, recentLog ) :: checkedLogs) nextMostRecent rest


insertLog : Log -> LoadedLogs -> LoadedLogs
insertLog newLog ( logs, unloadedLogs ) =
    ( case logs of
        [] ->
            [ ( newLog.date, ( newLog, [] ) ) ]

        recentLog :: rest ->
            insertLogHelper newLog [] recentLog rest
    , unloadedLogs
    )


insertLogHelper : Log -> List ( Time.Posix, NonEmptyList Log ) -> ( Time.Posix, NonEmptyList Log ) -> List ( Time.Posix, NonEmptyList Log ) -> List ( Time.Posix, NonEmptyList Log )
insertLogHelper newLog checkedLogs ( recentLogTime, recentLog ) toCheckLogs =
    if Time.posixToMillis newLog.date > Time.posixToMillis recentLogTime then
        List.reverse checkedLogs ++ ( newLog.date, ( newLog, [] ) ) :: ( recentLogTime, recentLog ) :: toCheckLogs

    else
        case toCheckLogs of
            [] ->
                List.reverse checkedLogs ++ [ ( newLog.date, ( newLog, [] ) ), ( recentLogTime, recentLog ) ]

            nextMostRecent :: rest ->
                insertLogHelper newLog (( recentLogTime, recentLog ) :: checkedLogs) nextMostRecent rest


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
                    Html.div [ Css.loader ] []

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
        ]
    }


viewSettings : FrontendModel -> Html FrontendMsg
viewSettings model =
    modal { isOpen = model.settingsOpen, onClose = UserClickedSettingsClose }
        []
        [ Html.h2 []
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
                    [ Html.Attributes.value model.owner ]
                    []
                ]
            , Html.label []
                [ Html.text "Repo"
                , Html.input
                    [ Html.Attributes.value model.repo ]
                    []
                ]
            , Html.label []
                [ Html.text "Token"
                , Html.input
                    [ Html.Attributes.value model.githubToken
                    , Html.Attributes.type_ "password"
                    ]
                    []
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
                , Html.button
                    [ Html.Attributes.type_ "submit" ]
                    [ Html.text "Log entry" ]
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
                Html.div [ Css.loader ] []

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


viewLog : ( Time.Posix, NonEmptyList Log ) -> ( String, Html FrontendMsg )
viewLog ( createdAt, ( log, _ ) ) =
    ( createdAt |> Time.posixToMillis |> String.fromInt
    , Html.form [ Css.previousLogTitle ]
        [ Html.label [ Css.previousLogTitle ]
            [ Html.input
                [ Html.Attributes.value log.title
                , Html.Attributes.placeholder "Title"
                ]
                []
            ]
        , Html.label [ Css.previousLogContent ]
            [ Html.textarea
                [ Html.Attributes.value log.content
                , Html.Attributes.placeholder "Content"
                ]
                []
            ]
        ]
    )


modal : { isOpen : Bool, onClose : msg } -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
modal options attributes =
    Html.node "dialog"
        (Html.Attributes.property "___wolfadex_modal__open" (Json.Encode.bool (Debug.log "isOpen" options.isOpen))
            :: Html.Events.on "close" (Json.Decode.succeed options.onClose)
            -- :: (if options.isOpen then
            --         Html.Attributes.attribute "open" ""
            --     else
            --         Html.Attributes.class ""
            --    )
            :: attributes
        )
