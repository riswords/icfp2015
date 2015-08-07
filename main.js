var path = require("path");
var Elm  = require("elm-loader");
var compiledCode = Elm(path.resolve(__dirname, "elm_main.elm"), __dirname);

compiledCode.emitter.on("output", function(message) {
    console.log(message)
});
