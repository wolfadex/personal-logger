const local_storage = require("./elm-pkg-js/local-storage");
console.log("Carl", -2);
exports.init = async function init(app) {
    // @WARNING: this only runs for Lamdera production deploys!
    // This file will not run in Local development, an equivalent to this is
    // automatically generated in Local Development for every file in elm-pkg-js/
    console.log("Carl", -1);
    local_storage.init(app);
};
