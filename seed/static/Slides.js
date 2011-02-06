var slides = $('.slide');

var currSlide = 0;
var numSlides = slides.length;

var fullWidth = slides.eq(0).outerWidth() + 32; // space between slides

var inState       = { left: 0 }
var outRightState = { left: fullWidth }
var outLeftState  = { left: -fullWidth }
var speed = 'fast';

for (i in slides) {
    if (i > 0) slides.eq(i).css({left: fullWidth});
}

var out
var gotoSlide = function (nextSlide) {
    if (nextSlide < 0) nextSlide = 0;
    if (nextSlide >= numSlides) nextSlide = numSlides - 1;
    if (nextSlide == currSlide) return;
    
    var cs = slides.eq(currSlide);
    var ns = slides.eq(nextSlide);
    
    if (nextSlide > currSlide) {
        ns.css(outRightState);
        cs.animate(outLeftState, speed);
    }
    else {
        ns.css(outLeftState);
        cs.animate(outRightState, speed);
    }
    ns.animate(inState, speed);
    
    currSlide = nextSlide;
}

$('#prev-slide').click(function(){ gotoSlide(currSlide - 1); return false; })
$('#next-slide').click(function(){ gotoSlide(currSlide + 1); return false; })
