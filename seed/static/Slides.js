var slides = $('.slide');

for (i in slides) {
    if (i > 0) slides.eq(i).hide();
}

var currSlide = 0;
var numSlides = slides.length;

var fullWidth = slides.eq(0).css('width');

var inState       = { opacity: 'show', left: 0, width: fullWidth }
var outRightState = { opactiy: 'hide', left: fullWidth,  width: 0 }
var outLeftState  = { opactiy: 'hide', left: 0,  width: 0 }

var out
var gotoSlide = function (nextSlide) {
    if (nextSlide < 0) nextSlide = 0;
    if (nextSlide >= numSlides) nextSlide = numSlides - 1;
    if (nextSlide == currSlide) return;
    
    var cs = slides.eq(currSlide);
    var ns = slides.eq(nextSlide);
    
    if (nextSlide > currSlide) {
        ns.css(outRightState);
        cs.animate(outLeftState, 'fast');
    }
    else {
        ns.css(outLeftState);
        cs.animate(outRightState, 'fast');
    }
    ns.animate(inState, 'fast');
    
    currSlide = nextSlide;
}

$('#prev-slide').click(function(){ gotoSlide(currSlide - 1); return false; })
$('#next-slide').click(function(){ gotoSlide(currSlide + 1); return false; })
