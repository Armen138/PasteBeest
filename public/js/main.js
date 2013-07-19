$(function() {
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
    for(var i = 0; i < CodeMirror.modeInfo.length; i++) {
        var option = "<option value='" + CodeMirror.modeInfo[i].mode + "'";
        if(CodeMirror.modeInfo[i].mode === language) {
            option += " selected=selected "
        }
        option += ">" + CodeMirror.modeInfo[i].name + "</option>";
        $("#language").append(option);
    }
    $("#language").change(function() {
        var mode = $(this).val();
        editor.setOption("mode", mode);
    });
});