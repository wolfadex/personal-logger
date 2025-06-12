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
    }


type alias LoadedLogs =
    ( List ( Time.Posix, NonEmptyList Log )
    , List ( Time.Posix, String )
    )


type LogResult
    = Unloaded
    | Loading (Maybe LoadedLogs)
    | Loaded LoadedLogs
    | Failure String


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


type ToBackend
    = NoOpToBackend
    | TB_LoadLogs StorageDetails
    | TB_CreateNewLog StorageDetails Bool { title : String, content : String }


type alias StorageDetails =
    { owner : String
    , repo : String
    , token : String
    }


type BackendMsg
    = NoOpBackendMsg
    | LogsLoadResponse Lamdera.SessionId (Result Http.Error LoadedLogs)
    | CreateNewLog Lamdera.SessionId StorageDetails Bool { title : String, content : String } Time.Posix
    | LogCreated Lamdera.SessionId Log (Result Http.Error ())


type ToFrontend
    = NoOpToFrontend
    | TF_LogsLoaded (Result String LoadedLogs)
    | TF_InsertLog Log
