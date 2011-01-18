$(document).ready(function () {

var bEnable = function(i) {
    $(i).removeAttr('disabled').animate({opacity: 1.0}, 'fast');
};

var bDisable = function(i) {
    $(i).attr('disabled', 'disabled').animate({opacity: 0.2}, 'fast');
}

var editable = false;

var mkEditable = function() {
    if (!editable) {
        bEnable('.btn-cancel');
        bEnable('.btn-save');
        $('#editor').removeClass('readonly').addClass('editable');
        editable = true;
    }
};

var mkReadOnly = function() {
    bDisable('.btn-cancel');
    bDisable('.btn-save');
    $('#editor').removeClass('editable').addClass('readonly');
    editable = false;
};


var editor = CodeMirror.fromTextArea("txt-src", {
    basefiles: ["/static/codemirror_base_min.js"],
    parserfile: ["/static/codemirror_parse_haskell.js"],
    stylesheet: "/static/codemirror.css",
    autoMatchParens: true,
    textWrapping: false,
    lineNumbers: true,
    indentUnit: 4,
    tabMode: "shift",
    enterMode: "keep",
    minHeight: 160,
    height: "dynamic",
    markParen: function(node, ok) {
        $(node).addClass(ok ? "paren-match" : "paren-error"); },
    unmarkParen: function(node) {
        $(node).removeClass("paren-match").removeClass("paren-error"); },
    cursorActivity: function(node) {
        var sel = editor.selection();
        if (!sel) { sel = node.innerText || node.textContent; }
        var mat = sel.match(/\S(.*\S)?/);
        if (mat) { $('.research-query').val(mat[0]); }
        },
    onChange: mkEditable
});

$('.btn-cancel').click(mkReadOnly);
mkReadOnly();

$('.panel h1').click(function () { $('.panel-content').slideToggle('fast'); });

var previewUrl = $('#preview-url').text();

var compileResult = function(data, status, xhr) {
    if (data == "OK") {
        $('.with-preview iframe').attr('src', previewUrl);
        $('.with-preview').show();
    }
    else {
        $('#errors').text(data);
        $('.with-errors').show();
    }
}

if (previewUrl) {
    $.ajax({
        url: previewUrl + "?__compile_only=1",
        success: compileResult,
        dataType: "text"
        });
}

})
