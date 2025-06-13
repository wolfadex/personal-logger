/* elm-pkg-js
port wolfadex__local_storage_get : String -> Cmd msg
port wolfadex__local_storage_gotten : (( String, Maybe String ) -> msg) -> Sub msg
port wolfadex__local_storage_set : ( String, String ) -> Cmd msg
port wolfadex__local_storage_delete : String -> Cmd msg
*/

exports.init = async function (app) {
    app.ports.wolfadex__local_storage_get.subscribe(function (key) {
        const value = localStorage.getItem(key);

        app.ports.wolfadex__local_storage_gotten.send([key, value]);
    });

    app.ports.wolfadex__local_storage_set.subscribe(function ([key, value]) {
        localStorage.setItem(key, value);
    });

    app.ports.wolfadex__local_storage_delete.subscribe(function (key) {
        localStorage.removeItem(key);
    });
};
