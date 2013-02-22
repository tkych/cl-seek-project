;;;; Last modified : 2013-02-22 19:42:37 tkych

;; cl-seek-project/package.lisp


;;====================================================================
;; Package for CL-SEEK-PROJECT
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-seek-project
  (:use :cl :iterate)
  (:import-from #:anaphora #:aif #:awhen #:it)
  (:export #:seek))

;;====================================================================
