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

$('.btn-cancel').click(mkReadOnly);
mkReadOnly();



var adjustColumnMarker = function () {
  var m = $('.editor-column-marker');
  var x = "0123456789";
  x = x + x + x + x + x + x + x + x;
  m.text(x);
  var w = m.innerWidth();
  m.text('');
  m.css('width', '10em');
  var v = m.width();
  m.css({ left: (w/v*10)+"em", width: '' });
}


var preview = $('#preview');
var editor = $('#editor');
var editImage = $('#rocker-edit-image');
var runImage = $('#rocker-run-image');

var toggle = function(iIn, iOut) {
    iIn.fadeIn('fast');
    iOut.fadeOut('fast');
}

var showHide = function(pShow, pHide, iIn, iOut) {
    pShow.slideDown('fast');
    pHide.slideUp('fast');
    toggle(iIn, iOut);
}

var mkRun = function () {
    preview.slideDown('fast');
    editor.slideUp('fast');
    toggle(runImage, editImage);
}
var mkEdit = function() {
    preview.slideUp('fast');
    editor.slideDown('fast', function() {
        cmEditor.refresh();
        adjustColumnMarker();
    });
    toggle(editImage, runImage);
 }

var submit = function() {
    $('#editor form').submit();    
}
    
var run = function() {
        toggle(runImage, editImage);
        if (editable) submit();
        else          mkRun();
    }
var recompile = function() {
        submit();
        return false;
    }

$('#rocker-run').click(run);
$('#rocker-edit').click(mkEdit);
$('#recompile').click(recompile);

$('.panel h1').click(function () { $('.panel-content').slideToggle('fast'); });



var cmEditor = CodeMirror.fromTextArea($("#txt-src")[0], {
    indentUnit: 4,
    enterMode: "keep",
    lineNumbers: true,
    matchBrackets: true,
    mode: 'haskell',
    
    //onChange: mkEditable,
    onCursorActivity: function() {
        var sel = cmEditor.getSelection();
        if (!sel) return;
        var mat = sel.match(/\S(.*\S)?/);
        if (mat) { $('.research-query').val(mat[0]); }
    },
});

cmEditor.setOption('onChange', mkEditable);

$('<div class="editor-column-marker"></div>')
    .appendTo($('.CodeMirror-lines > div:first'));


var previewUrl = $('#preview-url').text();

var setUpErrors = function (data) {
    var errorDetails = [];
    var currDetail;
    
    var r = /\.hs:(\d+):(\d+):$/;
    var dataLines = data.split("\n")
    for (var i in dataLines) {
        var e = dataLines[i].match(r);
        if (e) {
            var n = parseInt(e[1]);
            if (errorDetails[n]) {
                currDetail = errorDetails[n]
            }
            else {
                cmEditor.setMarker(n-1,'','error-line');
                currDetail = $('<pre class="error-details"></pre>')
                currDetail.appendTo($('body'));
                errorDetails[n] = currDetail;
            }
        }
        else if (currDetail) {
            var l = dataLines[i];
            if (l.slice(0,4) == '    ') { l = l.slice(4); }
            currDetail.text(currDetail.text() + l + "\n");
        }
    }

    $('.error-line').live('mouseenter', function (ev) {
            var lineNumber = $(ev.target);
            var detail = errorDetails[parseInt(lineNumber.text())];
            if (detail) {
                var w = lineNumber.offset();
                w.top  += lineNumber.height();
                w.left += lineNumber.width() * 1.3;
                detail.css(w);
                detail.show('fast');
            }
        });
    $('.error-line').live('mouseleave', function () {
            $('.error-details').hide('fast');
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
        setTimeout(function () { setUpErrors(data); }, 500);
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
else {
    mkEdit();
}

})
