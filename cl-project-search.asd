;;;; Last modified : 2013-02-21 21:51:48 tkych

;; cl-project-search/cl-project-search.asd


;;====================================================================
;; CL-PROJECT-SEARCH: Search in Quicklisp, Cliki, Github-repos
;;====================================================================
;; cl-project-search/
;;   cl-project-search.asd
;;   package.lisp
;;   seek.lisp
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
  :version     "0.1.01"
  :licence     "MIT License"
  :author      "Takaya Ochiai <tkych.repl@gmail.com>"
  :depends-on  (#:cl-ppcre #:iterate #:anaphora #:drakma
                #:flexi-streams #:yason #:do-urlencode #:html-entities)
  :serial      t
  :components  ((:file "package")
                (:file "seek"))
  )

;;====================================================================
