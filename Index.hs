module Template where

import Text.Html

page =
    thehtml <<
    header << [
        thelink ! [href "public/scaffold.css", rel "stylesheet",
                   thetype "text/css"] << noHtml,
        h1 << "Welcome aboard!"
        ]
