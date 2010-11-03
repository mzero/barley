bEnable = function(i) {
    $(i).removeAttr('disabled').animate({opacity: 1.0}, 'fast');
};

bDisable = function(i) {
    $(i).attr('disabled', 'disabled').animate({opacity: 0.2}, 'fast');
}

editor = null;

buildEditor = function(readOnly) {
    if (editor) {
        editor.toTextArea();
        editor = null;
    }
    var hasPreview = $('#preview').size() != 0;
    editor = CodeMirror.fromTextArea("txt-src", {
        basefiles: ["/static/codemirror_base_min.js"],
        parserfile: ["/static/codemirror_parse_haskell.js"],
        stylesheet: "/static/codemirror.css",
        autoMatchParens: true,
        textWrapping: false,
        lineNumbers: true,
        indentUnit: 4,
        tabMode: "shift",
        enterMode: "keep",
        readOnly: readOnly,
        minHeight: 160,
        height: (readOnly && hasPreview) ? "20em" : "dynamic",
        markParen: function(node, ok) {
            $(node).addClass(ok ? "paren-match" : "paren-error"); },
        unmarkParen: function(node) {
            $(node).removeClass("paren-match").removeClass("paren-error"); },
        cursorActivity: function(node) {
            var sel = editor.selection();
            if (!sel) { sel = node.innerText || node.textContent; }
            var mat = sel.match(/\S(.*\S)?/);
            if (mat) { $('.research-query').val(mat[0]); }
            }
    });
}
mkEditable = function() {
    $('#txt-src').removeAttr('readonly');
    buildEditor(false);
    bDisable('#btn-edit');
    bEnable('#btn-cancel');
    bEnable('#btn-save');
    $('#editor').removeClass('readonly').addClass('editable');
};

mkReadOnly = function() {
    $('#txt-src').attr('readonly', 'readonly');
    buildEditor(true);
    bEnable('#btn-edit');
    bDisable('#btn-cancel');
    bDisable('#btn-save');
    $('#editor').removeClass('editable').addClass('readonly');
};

$('#btn-edit').click(mkEditable);
$('#btn-cancel').click(mkReadOnly);
mkReadOnly();
