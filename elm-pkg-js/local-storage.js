/* elm-pkg-js
port wolfadex__local_storage_get : String -> Cmd msg
port wolfadex__local_storage_gotten : (( String, Maybe String ) -> msg) -> Sub msg
port wolfadex__local_storage_set : ( String, String ) -> Cmd msg
port wolfadex__local_storage_delete : String -> Cmd msg
*/
console.log("Carl", 0);
exports.init = async function (app) {
    console.log("Carl", 1);
    app.ports.wolfadex__local_storage_get.subscribe(function (key) {
        console.log("Carl", 2, key);
        const value = localStorage.getItem(key);
        console.log("Carl", 3, value);
        app.ports.wolfadex__local_storage_gotten.send([key, value]);
    });

    app.ports.wolfadex__local_storage_set.subscribe(function ([key, value]) {
        console.log("Carl", 4, key, value);
        localStorage.setItem(key, value);
    });

    app.ports.wolfadex__local_storage_delete.subscribe(function (key) {
        localStorage.removeItem(key);
    });
};
