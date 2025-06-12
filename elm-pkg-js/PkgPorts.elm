port module PkgPorts exposing (..)

-- This file is an example of what `elm-pkg-js` would generate for the user


ports =
    { wolfadex__local_storage_get = wolfadex__local_storage_get
    , wolfadex__local_storage_gotten = wolfadex__local_storage_gotten
    , wolfadex__local_storage_set = wolfadex__local_storage_set
    , wolfadex__local_storage_delete = wolfadex__local_storage_delete
    }


port wolfadex__local_storage_get : String -> Cmd msg


port wolfadex__local_storage_gotten : (( String, Maybe String ) -> msg) -> Sub msg


port wolfadex__local_storage_set : ( String, String ) -> Cmd msg


port wolfadex__local_storage_delete : String -> Cmd msg
