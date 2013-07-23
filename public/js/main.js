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
