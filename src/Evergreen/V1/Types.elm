module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Http
import Lamdera
import Time
import Url


type Submittable a
    = Fresh a
    | Submitting a
    | Issue a String


type Modifiable
    = Unmodified String
    | Modified
        { original : String
        , modified : String
        }


type alias EditableLog =
    { date : Time.Posix
    , title : Modifiable
    , content : Modifiable
    }


type alias Log =
    { date : Time.Posix
    , title : String
    , content : String
    }


type alias EditableLogList =
    { timestamp : Time.Posix
    , currentLog : Submittable EditableLog
    , logHistory : List Log
    , sha : String
    }


type alias UnloadedLog =
    { timestamp : Time.Posix
    , downloadUrl : String
    , sha : String
    }


type alias LoadedLogs =
    ( List EditableLogList, List UnloadedLog )


type LogResult
    = Unloaded
    | Loading (Maybe LoadedLogs)
    | Loaded LoadedLogs
    | Failure (Maybe LoadedLogs) String


type Field raw parsed
    = Untouched raw
    | Editing raw
    | Committed raw (Result String parsed)


type Theme
    = NoTheme
    | Theme_1
    | Theme_2
    | Theme_3
    | Theme_4
    | Theme_5
    | Theme_6


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , newLog :
        Submittable
            { title : String
            , content : String
            }
    , logs : LogResult
    , settingsOpen : Bool
    , owner : Field String String
    , repo : Field String String
    , token : Field String String
    , theme : Theme
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoadLogs
    | CreateLog
    | OwnerChanged String
    | OwnerBlurred
    | RepoChanged String
    | RepoBlurred
    | TokenChanged String
    | TokenBlurred
    | NewLogTitleChanged String
    | NewLogContentChanged String
    | ExistingLogTitleChanged Time.Posix String
    | ExistingLogContentChanged Time.Posix String
    | SubmitExistingChange Time.Posix
    | LoadMoreLogs
    | UserClickedSettingsOpen
    | UserClickedSettingsClose
    | SetTheme Theme
    | LocalStorageKeyReceived ( String, Maybe String )


type alias StorageDetails =
    { owner : String
    , repo : String
    , token : String
    }


type ToBackend
    = TB_LoadLogs StorageDetails
    | TB_CreateNewLog
        StorageDetails
        Bool
        { title : String
        , content : String
        }
    | TB_LoadMoreLogs StorageDetails (List UnloadedLog)
    | TB_SubmitExistingChange StorageDetails EditableLogList


type BackendMsg
    = LogsLoadResponse Lamdera.SessionId (Result Http.Error LoadedLogs)
    | MoreLogsLoadResponse Lamdera.SessionId (Result Http.Error (List EditableLogList))
    | CreateNewLog
        Lamdera.SessionId
        StorageDetails
        Bool
        { title : String
        , content : String
        }
        Time.Posix
    | LogCreated Lamdera.SessionId Log (Result Http.Error String)
    | StoreExistingChange Lamdera.SessionId StorageDetails EditableLogList Time.Posix
    | LogStored Lamdera.SessionId ( ( Time.Posix, String ), EditableLog, List Log ) (Result Http.Error ())


type ToFrontend
    = TF_LogsLoaded (Result String LoadedLogs)
    | TF_MoreLogsLoaded (Result String (List EditableLogList))
    | TF_InsertLog Log (Result String String)
    | TF_LogUpdated EditableLogList (Result String ())
