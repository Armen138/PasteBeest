;(function(e,t,n){function i(n,s){if(!t[n]){if(!e[n]){var o=typeof require=="function"&&require;if(!s&&o)return o(n,!0);if(r)return r(n,!0);throw new Error("Cannot find module '"+n+"'")}var u=t[n]={exports:{}};e[n][0].call(u.exports,function(t){var r=e[n][1][t];return i(r?r:t)},u,u.exports)}return t[n].exports}var r=typeof require=="function"&&require;for(var s=0;s<n.length;s++)i(n[s]);return i})({1:[function(require,module,exports){
(function() {
    var lang = require("./lang");
    var hist = require("./history");
    console.log(lang.list);
    console.log(hist.pastes);
    console.log(document.referrer);
    if (views === 1) {
        hist.add(window.location.pathname);
    }

    for(var paste in hist.pastes) {
        $("#sidepanel").append("<li>" + paste + "</li>");
    }
    $(function() {
        var languageSet = false;
        var editor = CodeMirror.fromTextArea(document.getElementById("code"), {
            lineNumbers: true,
            theme: "ambiance",
            mode: language
        });
        editor.setSize("100%", ($(window).height() - 34));
        $("#new").click(function(e) {
            e.preventDefault();
            window.location = "/";
        });
        $("#menu").click(function(e) {
            if($("#sidepanel").is(":visible")) {
                $("#sidepanel").slideUp({duration: 500 });
            } else {
                $("#sidepanel").slideDown({duration: 500 });
            }
        });
        for(var i = 0; i < lang.list.length; i++) {
            var option = "<option value='" + lang.list[i].mode + "'";
            if(lang.list[i].mode === language) {
                option += " selected=selected ";
            }
            option += ">" + lang.list[i].name + "</option>";
            $("#language").append(option);
        }
        $("#language").change(function() {
            languageSet = true;
            var mode = $(this).val();
            editor.setOption("mode", mode);
        });
        editor.on("change", function(doc, change) {
            if(!languageSet) {
                var detected = lang.detect(doc.getValue());
                $("#language").val(detected);
                editor.setOption("mode", detected);
            }
        });
    });
}());

},{"./lang":2,"./history":3}],2:[function(require,module,exports){
function has(haystack, needle) {
    return (haystack.indexOf(needle) !== -1);
}

function detectLanguage(data) {
    var detected = "null";

    if(data.search(/<\w*>/g)  !== -1) {
        detected = "xml";
    }
    if(has(data, "SELECT") ||
        has(data, "INSERT")) {
        detected = "sql";
    }
    if( has(data, "nil") ||
        has(data, "local") ||
        has(data, "repeat")) {
        detected = "lua";
    }
    if (has(data, "var") &&
        has(data, "function")) {
        detected = "javascript";
    }
    if( has(data, "void") ||
        has(data, "int") ||
        has(data, "class") ||
        has(data,"malloc")) {
        detected = "clike";
    }
    if( has(data, "(ns") ||
        has(data, "(require")) {
        detected = "clojure";
    }
    if( has(data, "unless") ||
        has(data, "->")) {
        detected = "coffeescript";
    }
    if( data.search(/^\s*\./g) !== -1 ||
        data.search(/^\s*#/g) !== -1 ) {
        detected = "css";
    }
    if( detected === "css" && data.search(/^\s*\@/g)  !== -1) {
        detected = "less";
    }
    if( detected === "css" && data.search(/^\s*\$/g)  !== -1) {
        detected = "sass";
    }
    if( data.search(/^\+/g)  !== -1&&
        data.search(/^-/g)  !== -1) {
        detected = "diff";
    }
    if( data.search(/_\w*?_/g)  !== -1) {
        detected = "markdown";
    }
    if( has(data, "mdo") ||
        has(data, "newtype") ||
        has(data, "foreign") ||
        has(data, "-<<")) {
        detected = "haskell";
    }

    if( has(data, "$") &&
        has(data, "my")) {
        detected = "perl";
    }

    if( has(data, "def")) {
        detected = "python";
    }
    if( detected === "python" &&
        has(data, "end")) {
        detected = "ruby";
    }
    if( has(data, "<?php") ||
        has(data, "isset") ||
        has(data, "$_")) {
        detected = "php";
    }
    if( has(data, "fn") &&
        has(data, "let")) {
        detected = "rust";
    }
    if( has(data, "echo") &&
        has(data, "$(")) {
        detected = "shell";
    }

    console.log("Language detected: " + detected);
    return detected;
}
exports.list = [
  {name: 'C/C++', mode: 'clike'},
  {name: 'Clojure', mode: 'clojure'},
  {name: 'CoffeeScript', mode: 'coffeescript'},
  {name: 'CSS', mode: 'css'},
  {name: 'diff', mode: 'diff'},
  {name: 'Markdown', mode: 'markdown'},
  {name: 'Haskell', mode: 'haskell'},
  {name: 'JavaScript', mode: 'javascript'},
  {name: 'LESS', mode: 'less'},
  {name: 'Lua', mode: 'lua'},
  {name: 'Perl', mode: 'perl'},
  {name: 'PHP', mode: 'php'},
  {name: 'Plain Text', mode: 'null'},
  {name: 'Python', mode: 'python'},
  {name: 'Ruby', mode: 'ruby'},
  {name: 'Rust', mode: 'rust'},
  {name: 'Sass', mode: 'sass'},
  {name: 'Shell', mode: 'shell'},
  {name: 'SQL', mode: 'sql'},
  {name: 'XML', mode: 'xml'}
];

exports.detect = detectLanguage;
},{}],3:[function(require,module,exports){
var pastes = {
    pastes: {},
    add: function(id) {
        pastes.pastes[id] = Date.now();
        pastes.save();
    },
    save: function() {
        localStorage.history = JSON.stringify(pastes.pastes);
    },
    load: function() {
        if(localStorage.history) {
            pastes.pastes = JSON.parse(localStorage.history);
        }
    }
};

pastes.load();

for(var nom in pastes) {
    exports[nom] = pastes[nom];
}
// exports = pastes;
},{}]},{},[1])
;