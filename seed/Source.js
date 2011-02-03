$(document).ready(function () {

var marker = $('#run-source-js-once');
if (marker.length > 0) {
    marker.text(marker.text() + " Bug! ");
    return;
}
$('<div id="run-source-js-once" style="display: none;">Done.</div>').appendTo($('body'));


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
        $('#editor-box').removeClass('readonly').addClass('editable');
        editable = true;
    }
};

var mkReadOnly = function() {
    bDisable('.btn-cancel');
    bDisable('.btn-save');
    $('#editor-box').removeClass('editable').addClass('readonly');
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
    markParen: ["paren-match", "paren-error"],
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


var preview = $('#preview');
var editor = $('#editor');
var editImage = $('#rocker-edit-image');
var runImage = $('#rocker-run-image');

var showHide = function(pShow, pHide, iIn, iOut) {
    pShow.slideDown('fast');
    pHide.slideUp('fast');
    iIn.fadeIn('fast');
    iOut.fadeOut('fast');
}

var mkRun = function () { showHide(preview, editor, runImage, editImage); }
var mkEdit = function() { showHide(editor, preview, editImage, runImage); }

$('#rocker-run').click(function() {
        mkRun();
        if (editable) $('#editor form').submit();
    });
$('#rocker-edit').click(mkEdit);


$('.panel h1').click(function () { $('.panel-content').slideToggle('fast'); });

var previewUrl = $('#preview-url').text();

var setErrorDetailAdjust = function(ln, lnp) {
    ln.hover(function () {
            var w = ln.offset();
            w.top  += ln.height();
            w.left += ln.width() * 1.3;
            lnp.css(w);
            lnp.show('fast');
            },
        function () {
            lnp.hide('fast');
        });
}
var compileResult = function(data, status, xhr) {
    if (data == "OK") {
        if ($('#preview').length > 0) {
            $('#preview .panel-content').hide();
            $('#preview iframe').attr('src', previewUrl);
            setTimeout(function() {
                $('#preview .panel-content').show('fast');
                }, 500);            
            if ($('#preview-show').length > 0)  mkRun();
            else                                mkEdit();
        }
    }
    else {
        setTimeout(function () {
            var lns = $('.CodeMirror-line-numbers div');
            var ln, lnp, lnps = [];
            
            var r = /\.hs:(\d+):(\d+):$/;
            var dataLines = data.split("\n")
            for (var i in dataLines) {
                var e = dataLines[i].match(r);
                if (e) {
                    var n = e[1];
                    if (lnps[n]) {
                        lnp = lnps[n]
                    }
                    else {
                        ln = lns.eq(n-1);
                        ln.addClass('error-line');
                        lnp = $('<pre class="error-details"></pre>')
                        lnp.appendTo($('body'));
                        setErrorDetailAdjust(ln, lnp);
                        lnps[n] = lnp;
                    }
                }
                else if (lnp) {
                    var l = dataLines[i];
                    if (l.slice(0,4) == '    ') { l = l.slice(4); }
                    lnp.text(lnp.text() + l + "\n");
                }
            }
            
        }, 300);
        $('#errors .panel-content').text(data);
        $('#errors .panel-content').hide();
        $('#errors').show('fast');
        mkEdit();
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
