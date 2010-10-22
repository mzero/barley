bEnable = function(i) {
    $(i).removeAttr('disabled').animate({opacity: 1.0}, 'fast');
};

bDisable = function(i) {
    $(i).attr('disabled', 'disabled').animate({opacity: 0.2}, 'fast');
}

mkEditable = function() {
    $('#txt-src').removeAttr('readonly');
    bDisable('#btn-edit');
    bEnable('#btn-cancel');
    bEnable('#btn-save');
};

mkReadOnly = function() {
    $('#txt-src').attr('readonly', 'readonly');
    bEnable('#btn-edit');
    bDisable('#btn-cancel');
    bDisable('#btn-save');
};

setResearch = function(e) {
    if ('selectionStart' in e) {
        var len = e.selectionEnd - e.selectionStart;
        var sel = e.value.substr(e.selectionStart, len);
        var mat = sel.match(/\S(.*\S)?/);
        if (mat) {
            $('.research-query').val(mat[0]);
        }
    };
};

buildLineNos = function() {
    var ns = "";
    for (var i in $(this).get(0).innerHTML.split("\n")) {
        ns += (1*i + 1) + "\n";
    }
    var n = $("<pre>"+ns+"</pre>");
    n.attr("class", "linenos");
    
    $(this).wrap("<div class=\"with-linenos\" />")
     .before(n);
};

$('#txt-src')
    .select(function () { setResearch($(this).get(0)); })
    .elastic()
    .each(buildLineNos);
$('#btn-edit').click(mkEditable);
$('#btn-cancel').click(mkReadOnly);
mkReadOnly();
