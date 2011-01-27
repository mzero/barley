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

var setErrorDetailAdjust = function(ln, lnp) {
    ln.hover(function () {
            var offset = ln.offset();
            lnp.css({
                'position': "absolute",
                'top': offset.top + ln.height(),
                'left': offset.left + ln.width() * 1.3
                });
            lnp.show('fast');
            },
        function () {
            lnp.hide('fast');
        });
}
var compileResult = function(data, status, xhr) {
    if (data == "OK") {
        $('.with-preview .panel-content').hide();
        $('.with-preview iframe').attr('src', previewUrl);
        $('.with-preview').show('fast');
        setTimeout(function() {
            $('.with-preview .panel-content').show('slow');
            }, 500);
    }
    else {
        setTimeout(function () {
            var lns = $('.CodeMirror-line-numbers div');
            var ln, lnp;
            
            var r = /\.hs:(\d+):(\d+):$/;
            var dataLines = data.split("\n")
            for (var i in dataLines) {
                var e = dataLines[i].match(r);
                if (e) {
                    ln = lns.eq(e[1]-1);
                    ln.addClass('error-line');
                    $('body').append('<pre class="error-details"></pre>');
                    lnp = $('body').children().last();
                    setErrorDetailAdjust(ln, lnp);
                }
                else if (lnp) {
                    var l = dataLines[i];
                    if (l.slice(0,4) == '    ') { l = l.slice(4); }
                    lnp.text(lnp.text() + l + "\n");
                }
            }
        }, 1000);
        $('#errors').text(data);
        $('.with-errors .panel-content').hide();
        $('.with-errors').show('fast');
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
