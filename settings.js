var path = require("path");
var fs = require("fs");
var exists = path.existsSync ? path.existsSync.bind(path) : fs.existsSync.bind(fs);
exports.host = process.env.OPENSHIFT_MONGODB_DB_HOST || "127.0.0.1";
exports.port = process.env.OPENSHIFT_MONGODB_DB_PORT || "27017";
exports.db = "pastebeest";
if(exists("openshift.prefix")) {
    exports.prefix = fs.readFileSync("openshift.prefix");
} else {
    exports.prefix = "";
}

