;;;; Last modified : 2013-02-22 19:48:32 tkych

;; cl-seek-project/cl-seek-project.asd


;;====================================================================
;; CL-SEEK-PROJECT: Seek CL Project in Quicklisp, Cliki, Github-repos
;;====================================================================
;; cl-seek-project/
;;   cl-seek-project.asd
;;   package.lisp
;;   seek.lisp
;;   README.md
;;   LICENSE
;;   CHANGELOG


;;====================================================================
;; System for CL-SEEK-PROJECT
;;====================================================================

(in-package :cl-user)

(asdf:defsystem #:cl-seek-project
  :name        "cl-seek-project"
  :description "Seek for CL project in Quicklisp, Cliki, Github-repos."
  :version     "0.1.11"
  :licence     "MIT License"
  :author      "Takaya Ochiai <tkych.repl@gmail.com>"
  :depends-on  (#:cl-ppcre #:iterate #:anaphora #:drakma
                #:flexi-streams #:yason #:do-urlencode #:html-entities)
  :serial      t
  :components  ((:file "package")
                (:file "seek"))
  )

;;====================================================================
