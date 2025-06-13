module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Lamdera
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , newLog : Submittable { title : String, content : String }
    , logs : LogResult

    -- Settings
    , settingsOpen : Bool
    , owner : Field String String
    , repo : Field String String
    , token : Field String String
    , theme : Theme
    }


type Theme
    = NoTheme
    | Theme_1
    | Theme_2
    | Theme_3
    | Theme_4
    | Theme_5
    | Theme_6


type alias LoadedLogs =
    ( List EditableLogList
    , List UnloadedLog
    )


type alias UnloadedLog =
    { timestamp : Time.Posix
    , downloadUrl : String
    , sha : String
    }


type LogResult
    = Unloaded
    | Loading (Maybe LoadedLogs)
    | Loaded LoadedLogs
    | Failure (Maybe LoadedLogs) String


type Submittable a
    = Fresh a
    | Submitting a
    | Issue a String


submittableValue : Submittable a -> a
submittableValue submittable =
    case submittable of
        Fresh a ->
            a

        Submitting a ->
            a

        Issue a _ ->
            a


toSubmittable : Submittable a -> Submittable a
toSubmittable submittable =
    case submittable of
        Fresh a ->
            Submitting a

        Submitting a ->
            Submitting a

        Issue a _ ->
            Submitting a


type Modifiable
    = Unmodified String
    | Modified { original : String, modified : String }


type Field raw parsed
    = Untouched raw
    | Editing raw
    | Committed raw (Result String parsed)


type alias EditableLogList =
    { timestamp : Time.Posix
    , currentLog : Submittable EditableLog
    , logHistory : List Log
    , sha : String
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


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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


type ToBackend
    = TB_LoadLogs StorageDetails
    | TB_CreateNewLog StorageDetails Bool { title : String, content : String }
    | TB_LoadMoreLogs StorageDetails (List UnloadedLog)
    | TB_SubmitExistingChange StorageDetails EditableLogList


type alias StorageDetails =
    { owner : String
    , repo : String
    , token : String
    }


type BackendMsg
    = LogsLoadResponse Lamdera.SessionId (Result Http.Error LoadedLogs)
    | MoreLogsLoadResponse Lamdera.SessionId (Result Http.Error (List EditableLogList))
    | CreateNewLog Lamdera.SessionId StorageDetails Bool { title : String, content : String } Time.Posix
    | LogCreated Lamdera.SessionId Log (Result Http.Error String)
    | StoreExistingChange Lamdera.SessionId StorageDetails EditableLogList Time.Posix
    | LogStored Lamdera.SessionId ( ( Time.Posix, String ), EditableLog, List Log ) (Result Http.Error ())


type ToFrontend
    = TF_LogsLoaded (Result String LoadedLogs)
    | TF_MoreLogsLoaded (Result String (List EditableLogList))
    | TF_InsertLog Log (Result String String)
    | TF_LogUpdated EditableLogList (Result String ())
