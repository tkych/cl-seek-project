Last modified : 2013-01-26 19:18:21 tkych

Version: 0.1.00 (beta)


CL-PROJECT-SEARCH : search in Quicklisp, Cliki, Github
======================================================

Introduction
------------

Have you ever experienced the following?
When you are programming at the REPL, you launch the browser in order
to find a CL library.
However, as a result of accidental clicks, you spent wasted time.

CL-PROJECT-SEARCH is a library for you (and I).
Without leaving the REPL, we could find the seeking library.
Therefore we never waste time by looking at unnecessary web page.


Description
-----------

CL-PROJECT-SEARCH is a search-engine-interface for Common Lisp.
The function SEARCH-PROJECT searches for a CL project in Quicklisp,
Cliki, Github-Repos.


Examples
--------

```lisp
CL-REPL> (cl-project-search:search-project 'Supercalifragilisticexpialidocious)

SEARCH RESULT: "supercalifragilisticexpialidocious"
=> NIL

CL-REPL> (cl-project-search:search-project "graphviz")

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

 SEARCH-SPACE: Github-Repos
  cl-graphviz
  donuts
  cl-dot
  motion-grammar-kit
  protocl
  sn-explorer
  compass
  graph-utils
  wo-git-gui

=> T

CL-REPL> (cl-project-search:search-project "graphviz" :description? t :cliki? nil)

SEARCH-RESULT: "graphviz"

 SEARCH-SPACE: Quicklisp
  com.informatimago.common-lisp.graphviz

 SEARCH-SPACE: Github-Repos
  cl-graphviz
      Adaptaion of an outdated cl-graphviz by Attila Lendvai to the modern version of libgvc.so.6.
  donuts
      Graph Drawing DSL (or Graphviz Interface) for Common Lisp.
  cl-dot
      Common Lisp package for generating GraphViz (dot) files
  motion-grammar-kit
      Formal Language Tools for Robots
  protocl
      Native Common Lisp compiler infrastructure for Google's protocol buffers
  sn-explorer
      A simple web crawler for social networks
  compass
      Recursive, k-width tree clusterer based on the Cardinal Directions
  graph-utils
      graph analysis tools
  wo-git-gui
      WebGUI for git commit graphs, will mainly focus on trying to reduce the commit graph into manageable overviews

=> T

CL-REPL> (cl-project-search:search-project "graphviz" :url? t :description? t :github? nil :quicklisp? nil)

SEARCH-RESULT: "graphviz"

 SEARCH-SPACE: Cliki
  cl-graphviz
      http://www.cliki.net/cl-graphviz
      cl-graphviz is a CFFI interface to graphviz with cl-graph integration
  cl-dot
      http://www.cliki.net/cl-dot
      CL-DOT is a library for easily generating dot (Graphviz) output from arbitrary Lisp data
  clod
      http://www.cliki.net/clod
      CLOD is a Common Lisp doc-generator, similar to Albert, Cldoc and so on
  graph-utils
      http://www.cliki.net/graph-utils
      graph-utils:  graph analysis utilities for Common Lisp
  Grapher-Server
      http://www.cliki.net/Grapher-Server
      Grapher-Server is an AJAX application combining AllegroServe, CL-Graph and GraphViz by Gary King
  s-dot
      http://www.cliki.net/s-dot
      S-Dot by Martin Loetzsch is a Common Lisp interface to the 'dot' tool of the GraphViz graphics library

=> T
```    

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

0. SHELL$   `git clone https://github.com/tkych/cl-project-search.git`
1. CL-REPL> `(push #p"/path-to-cl-project-search/cl-project-search/" asdf:*central-registry*)`
2. CL-REPL> `(ql:quickload :cl-project-search)`


Referece Manual
---------------

#### [Function] SEARCH-PROJECT _search-word_ _&key_ _web?_ _description?_ _url?_ _cliki?_ _github?_ _quicklisp?_

Search for project with SEARCH-WORD in Quicklisp, Cliki, Github-Repos.
SEARCH-WORD must be strings or symbols (symbols will be converted to downcase-strings).
If WEB? is NIL, not search Cliki and Github-Repos.
If QUICKLISP? is NIL, not search Quicklisp (also CLIKI?, GITHUB?).
At least one search-space must be specified.
If DESCRIPTION? is T, display project's description (except for Quicklisp-search).
If URL? is T, display project's url (except for Quicklisp-search).

- Space in SEARCH-WORD:
  In case search-word contains space, Quicklisp-search is OR-search,
  whereas Cliki,Github-search is AND-search.
  e.g. (search-project "foo bar")
       quicklisp-searches "foo" OR "bar",
       cliki,github-searches "foo" AND "bar".

- Max number of search result:
  Quicklisp - not limited,
  Github    - 100,
  Cliki     -  50.


TODO
----

- SEARCH-PROJECT: add search-space (bitbucket, google-code, etc.)
- SEARCH-PROJECT: pprint: for discription


Author
------

- Takaya Ochiai  <#.(reverse "moc.liamg@lper.hcykt")>


License
-------

- MIT License

