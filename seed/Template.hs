module Template where

import Text.Html

nu = () -- DO NOT DELETE THIS

page = thehtml << [
    h1 << "Hi!",
    paragraph << "testing"
    ]
