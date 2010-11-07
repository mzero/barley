(function () {

var bEnable = function(i) {
    $(i).removeAttr('disabled').animate({opacity: 1.0}, 'fast');
};

var bDisable = function(i) {
    $(i).attr('disabled', 'disabled').animate({opacity: 0.2}, 'fast');
}

var editable = false;

buildEditor = function(readOnly) {
    if (editor) {
        editor.toTextArea();
        editor = null;
    }
};

var mkReadOnly = function() {
    bDisable('.btn-cancel');
    bDisable('.btn-save');
    //$('#editor').removeClass('editable').addClass('readonly');
    $('#editor').animate({padding: 0});
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

$('.with-preview h1').click(function () { $('#preview').slideToggle('fast'); });

})()
