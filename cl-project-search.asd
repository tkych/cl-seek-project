;;;; Last modified : 2013-01-12 10:41:38 tkych

;; cl-project-search/cl-project-search.asd


;;====================================================================
;; CL-PROJECT-SEARCH: Search for Quicklisp, Cliki, Github-repos
;;====================================================================
;; cl-project-search/
;;   cl-project-search.asd
;;   package.lisp
;;   project-search.lisp
;;   README.md
;;   LICENSE
;;   CHANGELOG


;;====================================================================
;; System for CL-PROJECT-SEARCH
;;====================================================================

(in-package :cl-user)

(asdf:defsystem #:cl-project-search
  :name        "cl-project-search"
  :description "Search for CL project in Quicklisp, Cliki, Github-repos."
  :version     "0.1.00"
  :licence     "MIT License"
  :author      "Takaya Ochiai <tkych.repl@gmail.com>"
  :depends-on  (#:cl-ppcre #:iterate #:anaphora #:drakma
                #:flexi-streams #:yason #:do-urlencode #:html-entities)
  :serial      t
  :components  ((:file "package")
                (:file "search-project"))
  )

;;====================================================================