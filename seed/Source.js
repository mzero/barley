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

$('#txt-src').elastic();
$('#btn-edit').click(mkEditable);
$('#btn-cancel').click(mkReadOnly);
mkReadOnly();
