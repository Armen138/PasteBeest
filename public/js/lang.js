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