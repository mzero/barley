module Template where

import Text.Html

page = thehtml << [
    h1 << "Hi!",
    paragraph << "testing",
    ulist << map (li <<) [ bold << "one", italics << "two", tt << "three"]
    ]
