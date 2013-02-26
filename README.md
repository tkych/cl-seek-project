Last modified : 2013-02-26 19:54:22 tkych

Version: 0.1.13 (beta)


CL-SEEK-PROJECT : in Quicklisp, Cliki, Github, BitBucket
========================================================

Introduction
------------

CL-SEEK-PROJECT is a search-engine-interface for Common Lisp.
The function SEEK searchs for cl project in Quicklisp, Cliki, Github-Repos, BitBucket-Repos,
then outputs search-results in REPL.


Depends-on
----------

- anaphora
- iterate
- cl-ppcre
- drakma
- flexi-streams
- yason
- do-urlencode
- html-entities


Download & Install
------------------

0. SHELL$   `git clone https://github.com/tkych/cl-seek-project.git`
1. CL-REPL> `(push #p"/path-to-cl-seek-project/cl-seek-project/" asdf:*central-registry*)`
2. CL-REPL> `(ql:quickload :cl-seek-project)`


Examples
--------

```lisp
CL-REPL> (use-package :seek-project)  ;'seek' and '*description-max-num-cols*' is exported symbol.
=> T

CL-REPL> (seek 'Supercalifragilisticexpialidocious)

SEARCH RESULT: "supercalifragilisticexpialidocious"
=> NIL

CL-REPL> (seek "graphviz")

SEARCH-RESULT: "graphviz"

 SEARCH-SPACE: Quicklisp
  com.informatimago.common-lisp.graphviz

 SEARCH-SPACE: Cliki
  cl-graphviz
  cl-dot
  clod
  graph-utils
  Grapher-Server
  s-dot

 SEARCH-SPACE: GitHub-Repos
  cl-graphviz
  cl-dot
  donuts
  protocl
  motion-grammar-kit
  sn-explorer
  compass
  graph-utils
  wo-git-gui

=> T

CL-REPL> (seek "graphviz" :description? t :cliki? nil)

SEARCH-RESULT: "graphviz"

 SEARCH-SPACE: Quicklisp
  com.informatimago.common-lisp.graphviz

 SEARCH-SPACE: GitHub-Repos
  cl-graphviz
      Adaptaion of an outdated cl-graphviz by Attila Lendvai to the modern
      version of libgvc.so.6.
  cl-dot
      Common Lisp package for generating GraphViz (dot) files
  donuts
      Graph Drawing DSL (or Graphviz Interface) for Common Lisp.
  protocl
      Native Common Lisp compiler infrastructure for Google's protocol buffers
  motion-grammar-kit
      Formal Language Tools for Robots
  sn-explorer
      A simple web crawler for social networks
  compass
      Recursive, k-width tree clusterer based on the Cardinal Directions
  graph-utils
      graph analysis tools
  wo-git-gui
      WebGUI for git commit graphs, will mainly focus on trying to reduce the
      commit graph into manageable overviews

=> T

CL-REPL> (setf *description-max-num-cols* 60) ;default is 80

=> 60

CL-REPL> (seek "graphviz" :url? t :description? t :cliki? nil :quicklisp? nil)

SEARCH-RESULT: "graphviz"

 SEARCH-SPACE: GitHub-Repos
  cl-graphviz
      https://github.com/necto/cl-graphviz
      Adaptaion of an outdated cl-graphviz by Attila Lendvai
      to the modern version of libgvc.so.6.
  cl-dot
      https://github.com/michaelw/cl-dot
      Common Lisp package for generating GraphViz (dot)
      files
  donuts
      https://github.com/tkych/donuts
      Graph Drawing DSL (or Graphviz Interface) for Common
      Lisp.
  protocl
      https://github.com/scymtym/protocl
      Native Common Lisp compiler infrastructure for
      Google's protocol buffers
  motion-grammar-kit
      https://github.com/golems/motion-grammar-kit
      Formal Language Tools for Robots
  sn-explorer
      https://github.com/GChristensen/sn-explorer
      A simple web crawler for social networks
  compass
      https://github.com/abutcher/compass
      Recursive, k-width tree clusterer based on the
      Cardinal Directions
  graph-utils
      https://github.com/kraison/graph-utils
      graph analysis tools
  wo-git-gui
      https://github.com/woudshoo/wo-git-gui
      WebGUI for git commit graphs, will mainly focus on
      trying to reduce the commit graph into manageable
      overviews

=> T
```    


Referece Manual
---------------

#### [Function] SEEK _search-word_ _&key_ _web?_ _description?_ _url?_ _cliki?_ _github?_ _quicklisp?_ _bitbucket?_

Search for cl project with _search-word_ in Quicklisp, Cliki, GitHub-Repos, BitBucket-Repos.
_search-word_ must be string or symbol (symbol will be coerced to downcase-string).
If _web?_ is NIL, it does not search Cliki, GitHub-Repos and BitBucket-Repos.
If _quicklisp?_ is NIL, it does not search Quicklisp (also _cliki?_, _github?_, _bitbucket?_).
At least one search-space must be specified.
If _description?_ is T, it displays project's description (except for Quicklisp-search).
If _url?_ is T, it displays project's url (except for Quicklisp-search).

- Space in _search-word_:
  If _search-word_ contains #\space, Quicklisp-search is OR-search,
  whereas Cliki,GitHub,BitBucket-search is AND-search.
  e.g. (seek "foo bar"):
       Quicklisp-search    - "foo" OR "bar",
       Cliki,GitHub,BitBucket-search - "foo" AND "bar".

- Max number of search result:
  Quicklisp - unlimited,
  GitHub    - 100,
  Cliki     - 50,
  BitBucket - 50.


#### [Special Variable] \*DESCRIPTION-MAX-NUM-COLS\*

If the length of description-string is bigger then \*description-max-num-cols\*, 
then search-result is inserted newline for easy to see.
Default value is 80.


TODO
----

- Add: search-space (google-code, etc.)


Author, License, Copyright
--------------------------

* Takaya Ochiai  <#.(reverse "moc.liamg@lper.hcykt")>

* MIT License

* Copyright (C) 2013 Takaya Ochiai
