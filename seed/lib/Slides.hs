module Slides (
    slideDeck,
    titleSlide,
    pointSlide
    ) where

import Text.Html

slideDeck :: String -> [Html] -> Html
slideDeck title slides =
    header <<
        [ thetitle << title
        , thelink ! [href "/static/slides.css", rel "stylesheet",
                   thetype "text/css"] << noHtml
        ]
    +++
    body << (slideArea : buttons : scripts)
  where
    buttons = thediv ! [ identifier "controls" ] <<
        [ anchor ! [ identifier "prev-slide", href "#" ] << "prev"
        , anchor ! [ identifier "next-slide", href "#" ] << "next"
        ]
    scripts = map mkScript [ "/static/jquery.js", "/static/Slides.js" ]
    mkScript s = tag "script" ! [ thetype "text/javascript", src s ] << noHtml
    slideArea = thediv ! [ identifier "slides" ] << slides

titleSlide :: String -> [String] -> Html
titleSlide heading subHeadings =
    thediv ! [ theclass "slide title-slide" ] <<
        (h1 << heading : map (h2 <<) subHeadings)

pointSlide :: String -> String -> [String] -> Html
pointSlide heading leadIn points =
    thediv ! [ theclass "slide bullet-slide" ] <<
        [ h1 << heading
        , if null leadIn then noHtml else p << leadIn
        , ulist << map (li <<) points
        ]
