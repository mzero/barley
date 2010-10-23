module DevUtils (
    devpage,
    ) where
    
import Text.Html


devpage :: HTML a => String -> a -> [Html] -> [String] -> Html
devpage pageTitle contents modules scriptSrcs =
    header << [
      thelink ! [href "static/scaffold.css", rel "stylesheet",
                   thetype "text/css"] << noHtml,
      thetitle << fullTitle
      ] +++
    body ! [theclass "with-topbar"] << [
      h1 ! [identifier "logo"] << "Barley",
      thediv ! [identifier "content", theclass "with-sidebar"] <<
        (toHtml contents +++ toHtml (sidebar modules)),
      topbar,
      scripts scriptSrcs
      ]
  where
    fullTitle = if null pageTitle then "Barley" else "Barley - " ++ pageTitle

sidebar :: [Html] -> Html
sidebar modules = thediv ! [identifier "sidebar"] <<
    map (thediv ! [theclass "module"]) modules

topbar :: Html
topbar = thediv ! [identifier "topbar"] << [
    p << makelink haskellLink,
    unordList $ map makelink communityLinks
    ]
  where
    makelink (title, url) = anchor ! [href url] << title
    haskellLink = ("Haskell", "http://haskell.org/")
    communityLinks =
        [ ("Platform", "http://hackage.haskell.org/platform/")
        , ("Hackage", "http://hackage.haskell.org/packages/hackage.html")
        , ("λ Reddit", "http://www.reddit.com/r/haskell/")
        , ("λ Stack Overflow",
            "http://stackoverflow.com/questions/tagged?tagnames=haskell")
        ]

scripts :: [String] -> Html
scripts = toHtml . map script
  where
    script s = tag "script" ! [ thetype "text/javascript", src s ] << noHtml
