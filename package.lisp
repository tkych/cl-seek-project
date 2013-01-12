;;;; Last modified : 2013-01-12 10:34:36 tkych

;; cl-project-search/package.lisp


;;====================================================================
;; Package for CL-PROJECT-SEARCH
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-project-search
  (:use :cl :iterate)
  (:import-from #:anaphora #:aif #:awhen #:it)
  (:export #:search-project))

;;====================================================================