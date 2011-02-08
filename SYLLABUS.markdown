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

Other Syllabi
-------------
LYAH's "Starting Out"
    numbers, negative numbers
    numeric expressions
    boolean expressions
    == and /=
    succ, min, max
    
    defining a simple function
    if then else
    let (in ghci)
    [1,2,3] notation
    ++
    "abc" is a list
    :
    !!
    nested lists
    comparisons on lists
    head, tail, last init
    length
    null
    reverse
    take drop
    
    maximum
    sum
    product
    elem
    `elem` notation
    [1..20] notation, and with chars, and with steps
    
    cycle, repeat, replicate
    
    list comprehension syntax
    
    2-tuples
    fst snd
    zip
    
    
    
RWH's "Starting Out"
    numbers, negative numbers
    numeric expressions
    boolean expressions
    ==, /= comparisons
    
    let (in ghci)
    
    [1,2,3] notation
    []
    [1..] notation, with steps
    ++ :
    strings, chars, lists
    
    script: main, interact, show, lines
    
    
RWH's "Types and Functions"
    Char, Bool, Int, Integer, Double
    function application
    odd, compare, sqrt
    head, tail, 
    2-tuples
    take, drop
    fst, snd
    lines
    
    declaration vs. assignment
    
    

bos' intro
-1-
    * functional is normal: computation / unix pipes
    * brain twisting goodness
    * basic expressions
    * simply program (compile, main)
    * function application
    * module edit/load cycle
    * wordCount program
    * sort program
    * lists
    * numberLines program
    * recursive function
    * HW: fgrep program (via isInfixOf)
-2-
    * list constructors
    * guards over if-then-else
    * pattern match (via myLength)
    * myLength, myHead, myTail
    * pattern match failure
    * strings are lists
    * more pattern matching
    * HW: nth, lastButOne, isPalindrome
-3-
    * types
    * basic types: Int, Char, Boolean, Double, Integer
    * type signatures ::
    * ...are optional
    * function types
    * functions with multiple args
    * tuples
    * fst3, snd3, thd3
    * list types, String
    * type synonyms
    * drop is polymorphic
    * type variables in signatures
    * the type of filter
    * type inference and bizarre outcomes
    * reading types
    * inferring from types
    * HW: myZipWith, ucFirst
-4-
    * data (via Quaternion)
    * constructors
    * using your type in a function
    * pattern match on constructor
    * _
    * :info, :type
    * compare w/C++, Python
    * equality of user type
    * operator as function, function as operator
    * typeclass --- Eq
    * instance of Eq for quaternion
    * class constraint on elem
    * enum-like data
    * Show class
    * deriving, Eq, ord, Bounded, Enum, Show
    * HW: :info those classes
    * HW: angle classification, convexHull
-5-
    * Num and Ord typeclasses, applied to Quaternion
    * typeclass laws... implied
    * failure and Maybe
    * safeDiv, safeHead
    * mapMaybe (where was map?)
    * binary trees
    * map, mapMaybe, mapTree --> Functor!
    * HW: tree insert, tree contains
-6-
    * short circuit operators
    * strict vs. non-strict
    * non-strict... as lazy... as call-by-need... w/memoization
    * finding the commonality in map and bunzip -> foldr
    * func. composition, lambda
    * foldl vs. foldr and stack overflow
    * seq and foldl'
    * HW: writing lots of things with folds
-7-
    * count words over files listed as args
    * do notation
    * imports
    * 



Chapter Goals
1: a numbered list
    -- syntax
    -- if-then-else
    -- basic functions: head, tail, ++, show
    -- recursion
    -- type annotations
    -- function application
    -- operators

2: a todo list
    -- HTML
    -- version w/if-then-else and recursion
    -- version w/guards
    -- version w/pattern matching
    -- version w/higher order functions (map)
    -- where clause
    -- develop the numbered list in concert with the todo list

3: todo list w/priority
    -- 2-tuple
    -- 3-tuple -> 
    -- data types
    -- 
    