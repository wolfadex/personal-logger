module LocalStorage exposing (..)

{-| Provides a simple way to hook up Javascript copy-to-clipboard functionality

@docs copy

-}


type alias PkgPorts ports msg =
    { ports
        | wolfadex__local_storage_get : String -> Cmd msg
        , wolfadex__local_storage_gotten : (( String, Maybe String ) -> msg) -> Sub msg
        , wolfadex__local_storage_set : ( String, String ) -> Cmd msg
        , wolfadex__local_storage_delete : String -> Cmd msg
    }


{-| Query for a value from localStorage
-}
get : PkgPorts a msg -> String -> Cmd msg
get ports =
    ports.wolfadex__local_storage_get


{-| Receive a value from localStorage
-}
gotten : PkgPorts a msg -> (( String, Maybe String ) -> msg) -> Sub msg
gotten ports =
    ports.wolfadex__local_storage_gotten


{-| Set a value int localStorage
-}
set : PkgPorts a msg -> String -> String -> Cmd msg
set ports key value =
    ports.wolfadex__local_storage_set ( key, value )


{-| Remove a value from localStorage
-}
delete : PkgPorts a msg -> String -> Cmd msg
delete ports =
    ports.wolfadex__local_storage_delete
