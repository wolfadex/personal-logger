module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Lamdera
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , logs : LogResult
    , owner : String
    , repo : String
    , githubToken : String
    , newLog : NewLog
    , settingsOpen : Bool
    }


type alias LoadedLogs =
    ( List ( Time.Posix, NonEmptyList Log )
    , List ( Time.Posix, String )
    )


type LogResult
    = Unloaded
    | Loading (Maybe LoadedLogs)
    | Loaded LoadedLogs
    | Failure (Maybe LoadedLogs) String


type NewLog
    = Fresh { title : String, content : String }
    | Submitting { title : String, content : String }
    | Issue { title : String, content : String } String


type alias NonEmptyList a =
    ( a, List a )


type alias Log =
    { date : Time.Posix
    , title : String
    , content : String
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | LoadLogs
    | CreateLog
    | NewLogTitleChanged String
    | NewLogContentChanged String
    | ExistingLogTitleChanged Time.Posix String
    | ExistingLogContentChanged Time.Posix String
    | LoadMoreLogs
    | UserClickedSettingsOpen
    | UserClickedSettingsClose


type ToBackend
    = NoOpToBackend
    | TB_LoadLogs StorageDetails
    | TB_CreateNewLog StorageDetails Bool { title : String, content : String }
    | TB_LoadMoreLogs StorageDetails (List ( Time.Posix, String ))


type alias StorageDetails =
    { owner : String
    , repo : String
    , token : String
    }


type BackendMsg
    = NoOpBackendMsg
    | LogsLoadResponse Lamdera.SessionId (Result Http.Error LoadedLogs)
    | MoreLogsLoadResponse Lamdera.SessionId (Result Http.Error (List ( Time.Posix, NonEmptyList Log )))
    | CreateNewLog Lamdera.SessionId StorageDetails Bool { title : String, content : String } Time.Posix
    | LogCreated Lamdera.SessionId Log (Result Http.Error ())


type ToFrontend
    = NoOpToFrontend
    | TF_LogsLoaded (Result String LoadedLogs)
    | TF_MoreLogsLoaded (Result String (List ( Time.Posix, NonEmptyList Log )))
    | TF_InsertLog Log
