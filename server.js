var mongo = require('mongodb').MongoClient;
var express = require("express");
var jade = require("jade");
var fs = require("fs");
var settings = require("./settings.js");
var app = express();
var index = jade.compile(fs.readFileSync("templates/index.jade"));

var record = {
    create: function(code, id, name, language) {
        return {
            id: id,
            name: name || "guest",
            code: code || "//no code here",
            language:  language || "javascript",
            timestamp: Date.now()
        };
    }
};

function main(err, db) {
    var collection = db.collection("pastes");
    app.set("title", "PasteBeest");
    app.use(express.logger());
    app.use(express.bodyParser());

    app.get(/^\/[\dA-F]+?$/, function(req, res) {
        var id = req.path.substr(1);
        collection.findOne({id: id}, function(err, doc) {
            if(err || doc === null) {
                res.status(404).send(index({"id": req.path.substr(1), "code": "Paste Not Found", "language": "javascript"}));
            } else {
                var body = index({"id": req.path.substr(1), "code": doc.code, "language": doc.language});
                res.setHeader("Content-Type", "text/html");
                res.setHeader("Content-Length", body.length);
                res.end(body);
            }
        });
    });

    app.get("/", function(req, res) {
        var body = index({"id": "", "code": "", "language": "javascript"});
        res.setHeader("Content-Type", "text/html");
        res.setHeader("Content-Length", body.length);
        res.end(body);
    });

    app.post("/", function(req, res) {
        collection.count(function(err, count) {
            var rec = record.create(req.body.code, count.toString(16).toUpperCase(), null, req.body.language);
            collection.insert(rec, function(err, docs) {
                res.redirect("/" + rec.id);
            });
        });
    });

    app.use(express.static(__dirname + '/public'));
    var ipaddr  = process.env.OPENSHIFT_NODEJS_IP || "127.0.0.1";
    var port    = process.env.OPENSHIFT_NODEJS_PORT || 9999;
    app.listen(port, ipaddr);
    console.log("PasteBeest Online at " + ipaddr + ":" + port);
}

mongo.connect('mongodb://' + settings.prefix + settings.host + ':' + settings.port + '/' + settings.db, main);

