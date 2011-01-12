Barley Syllabus
===============

This document is to collect the ideas for barley's tutorial and included
examples. At present it contains just some ramblings. We need to pull this
into a more researched and clear syllabus soon.

Sources of inspiration for this include:

* [Real World Haskell](http://book.realworldhaskell.org/)
* [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
    

Haskell Concepts
----------------

Barley should introduce a short list of Haskell concepts and idioms from a 
practical point of view. The aim is to get programmers looking at and
editing examples of these idioms that do something. We are less interested
in explaining the details or the philosophic underpinnings in the tutorial
than getting them to the _"OIC, wow, cool, k thx bai"_ stage.

* Functions
* Numbers
* Strings & Text
* `case`, `if`, patterns, guards
* Lists
* `map`, `filter`, and friends
* List comprehensions
* `Maybe` and `Either`
* `do` syntax, in so far as it can be used with `Maybe`, `Either`, and `List`
* `fmap`, in so far as it can be used with `Maybe`, `Either`, and `List`
* Currying and Point-free style _(gently)_
* `data` types
* `type` and `newtype`
* Parameterized types
* Type classes and instances
* IO
* Modules: importing and exporting


Web Concepts
------------

The tutorial's aim isn't to teach web programming. This is a list of web
concepts the tutorial should show how to do in Haskell. We are assuming
that most programmers coming to the tutorial have an inkling of how to do
these things in other languages.

* HTML generation
* Interleaving fixed HTML with generated content
* Getting query args
* Forms: generating, getting args and processing
* Referencing static elements
* Cookies
* Logging
* Read/Write simple file storage
* Read/Write SQLite storage
* Make outgoing HTTP requests
* Generate JSON


Web Projects
------------

All of the above needs to be presented in the context of one or two
extended web projects that the developer builds during the bulk of
the tutorial. In addition, some concepts can be demonstrated in six
to eight examples left for the developer to explore after they've
done the tutorial. This is a list of possible projects:

* Simple twitter-like web site: accounts, sign up, sign in, message,
  render messages per user, front page of recent messages
* Guess the animal game with growing DB of animals.
* Unit conversion, simple calculator page.
* News feed mash up: show reddit, slashdot, tech news headlines on a
  a single page with links
* Polls: admin can create polls, users take 'em
* Blog
* Wiki
* Inventory (recipies, wine, videos, todos, etc...): Item entry, item detail, listing,
query, categories, bools(?)



An Outline
----------

Ch 1: Static Pages
* Step 1: "Hello World"
    - comments, strings
    - basic html combinators & usage
* Step 2: "Shopping List"
    - lists
    - homogenous types (  [ "foo", bold << "bar" ]
    - toHtml
* Step 3: "Some Style"
    - first function, map, list comprehension
    - attributes, name exceptions
* Step 4: "
    - tuples, zip, ranges 
    
    
Ch 2: Guessing Game
* Step 1: Form
    - if/then/else, comparison
    - http request & response objects
* Step 2: Encoding state
    - string functions, appending
    - 