Barley - A seed for learning Haskell
====================================

Barley is an environment for tinkering with [Haskell][1]. It is a web server
with an embedded simple programming environment rendered as web pages.
The environment allows you to easily edit Haskell code and quickly see the
result of running it.

Barley includes a tutorial aimed at people with some programming and web
experience, but it assumes no exposure to Haskell or functional programming.
The tutorial takes the developer from generating a simple web page through
a modest web application with database backend storage. Along the way it
exposes the programmer to the basic idioms needed to do simple programming
tasks.

Barley is based on the [Snap framework][2] for creating web programming in
Haskell. Barley adds a simple web application skeleton to support both the
live development environment and the tutorial projects. After going through
the tutorial, developers can use Barley to create and explore their own
simple web projects. Beyond that, they can smoothly transition to using the
whole Snap framework and the wealth of available Haskell libraries.

[1]: http://www.haskell.org/
[2]: http://snapframework.com/

Status
======

This is in the *very early concept stage*. We aren't even at a 0.1 release!
Read this code at your own risk!

Developers
----------

Barley was conceived during a flurry of Nepalese food by [Johan Tibell][jt]
and [Mark Lentczner][ml]. We got immediate encouragement from others 'round
the table and spent the rest of the night plotting, and in Johan's case coding.
The next morning we found ourselves in a coffee house with other Haskellers
cheering us on as we knocked off the first coding tasks.

[jt]: http://github.com/tibbe
[ml]: http://github.com/mtnviewmark

Running
-------

If you can't hold back and download this code, you can run it like so:

    cabal configure
    cabal build
    ghci -isrc:dist/build/autogen src/Main.hs
    > :main start playground

Note:

* `start` = initialize a new project if needed, then run barley on it.
* `playground` is just the name of a project directory, barley will create
  it if needed.
* If you don't `cabal install` barley, then it won't have access to its
  data directory unless you start it from the root of the dev. tree.


License
=======

Copyright 2010 Google Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


Additional Credits
------------------

* [jQuery][c1] *-- MIT License*
* [CodeMirror][c2] by Marijn Haverbeke *-- zlib-style license*
* ["A Lady Beetle Perches on Barley"][c3] by T. Voekler
  *-- Creative Commons Attribution-ShareAlike 3.0 Unported*
* [Silk Icons][c4] by Mark James *-- Creative Commons Attribution 2.5*

[c1]: http://jquery.com/
[c2]: http://codemirror.net/
[c3]: http://commons.wikimedia.org/wiki/File:A_lady_beetle_perches_on_barley.JPG
[c4]: http://www.famfamfam.com/lab/icons/silk/
