module Slides (
    slideDeck,
    titleSlide,
    pointSlide,
    codeSlide,
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

codeSlide :: String -> String -> [String] -> Html
codeSlide heading leadIn codeLines =
    thediv ! [ theclass "slide code-slide" ] <<
        [ h1 << heading
        , if null leadIn then noHtml else p << leadIn
        , pre << unlines codeLines
        ]

class SlideItem a where
    render :: a -> Html
    renderList :: [a] -> Html

instance (SlideItem a) => SlideItem [a] where
    render = renderList
    renderList as = ulist << map ((li <<).render) as
    
instance SlideItem Char where 
    render = toHtml . (:[])
    renderList = toHtml

instance SlideItem Html where
    render = id
    renderList hs = ulist << map (li <<) hs

slide :: (SlideItem a) => String -> String -> a -> Html
slide heading leadIn content = 
    thediv ! [ theclass "slide" ] <<
        [ if null heading then noHtml else h1 << heading
        , if null leadIn then noHtml else p << leadIn
        , render content
        ]
