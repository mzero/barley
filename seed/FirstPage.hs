module FirstPage where

import Text.Html

-- FIRST:  Try toggling the Edit/Run button above this editor box back and forth.
-- SECOND: Try editing some text inside quotes, and hit Run.
-- THIRD:  If you are brave, try adding another paragraph, or an ordList.
-- FOURTH: Go back to the Home page (link at top right) and start the tutorial.

page = thehtml <<
    [ h1 << "Hi!"
    , paragraph << "This page was rendered from Haskell."
    , unordList [ bold << "one", italics << "two", tt << "three"]
    ]
