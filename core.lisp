;;;; Last modified : 2013-03-20 21:48:12 tkych

;; cl-seek-project/core.lisp


;;====================================================================
;; Core
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-seek-project
  (:nicknames #:seek-project)
  (:use :cl :iterate)
  (:import-from #:anaphora #:aif #:awhen #:it)
  (:export #:seek
           #:*max-num-description-columns*
           ;#:*max-num-web-search-results*
           ))

(in-package #:cl-seek-project)


;;--------------------------------------------------------------------
;; Seek
;;--------------------------------------------------------------------
(defparameter *description-print-p* t)
(defparameter *url-print-p* t)

;; !! TODO !! add: search-space google-code
;; !! TODO !! limit output num:
(defun seek (search-word &key (web? t) (description? nil) (url? nil)
                              ;(limit? 50)
                              (cliki? t) (github? t) (bitbucket? t)
                              (quicklisp? t))
  "Search for cl project with SEARCH-WORD in Quicklisp, Cliki, Github-Repos BitBucket-Repos.
SEARCH-WORD must be string or symbol (symbol will be converted to downcase-string).

If WEB? is NIL, it does not search in Cliki, Github-Repos and BitBucket-Repos.
If QUICKLISP? is NIL, it does not search in Quicklisp (also CLIKI?, GITHUB?, BITBUCKET?).
At least one search-space must be specified.

If DESCRIPTION? is T, it displays project's description (except for Quicklisp-search).
If URL? is T, it display project's url (except for Quicklisp-search).

N.B.
 * #\\Space in SEARCH-WORD:
   In case search-word contains #\\Space, Quicklisp-search is OR-search,
   whereas Cliki,GitHub,BitBucket-search is AND-search.

   e.g. (seek \"foo bar\")
        Quicklisp-search for \"foo\" OR \"bar\",
        Cliki,GitHub,BitBucket-search for \"foo\" AND \"bar\".

 * Max number of search result:
    Quicklisp - unlimited
    GitHub    - 50
    Cliki     - 50
    BitBucket - 50
"
  (unless (or (stringp search-word) (symbolp search-word))
    (error "~S is not string or symbol." search-word))
  (unless (or quicklisp?
              (and web? (or cliki? github?)))
    (error "There is no search-space."))
  (let ((found? nil)
        (*url-print-p* url?)
        (word-string
         (write-to-string search-word :case :downcase :escape nil)))
    (format t "~%SEARCH-RESULTS: ~S~%" word-string)

    #+quicklisp   ; for quicklisp build
    (when (and quicklisp? (search-quicklisp word-string))
      (setf found? t))

    (when web?
      (let ((drakma:*drakma-default-external-format* :utf-8)
            ;(*max-num-web-search-results* limit?)
            (*description-print-p* description?))
        (when (and cliki? (search-cliki word-string))
          (setf found? t))
        (when (and github? (search-github word-string))
          (setf found? t))
        (when (and bitbucket? (search-bitbucket word-string))
          (setf found? t))))
    (terpri)
    found?))


;;--------------------------------------------------------------------
;; Quicklisp search
;;--------------------------------------------------------------------
#+quicklisp  ; for quicklisp build
(progn
  (defun search-quicklisp (word-string)
    (let ((found? nil))
      (dolist (wd (ppcre:split " " word-string))
        (awhen (search-ql-systems wd)
          (unless found?
            (format t "~% SEARCH-SPACE: Quicklisp~%")
            (setf found? t))
          (print-ql-results it)))
      found?))

  (defun search-ql-systems (word-string)
    (loop :for system :in (ql-dist:provided-systems t)
          :when (or (search word-string (ql-dist:name system))
                    (search word-string
                            (ql-dist:name (ql-dist:release system))))
          :collect system))

  (defun print-ql-results (systems)
    (dolist (system systems)
      (format t "~&  ~A" (ql-dist:name system))
      ;; (when *url-print-p*
      ;;   (format t "~%      ~A"
      ;;           ))
      ;; (when *description-print-p*
      ;;   (format t "~%      ~A"
      ;;           (ql-dist:short-description system)))
      (terpri)))

  ) ;end of #+quicklisp


;;--------------------------------------------------------------------
;; web search common functions
;;--------------------------------------------------------------------
(defparameter *max-num-web-search-results* 50
  "")

(defparameter *num-results-per-page* 10)

(defun max-num-next-pages ()
  (ceiling (- *max-num-web-search-results* *num-results-per-page*)
           *num-results-per-page*))

(defparameter *max-num-description-columns* 80
  "If the length of description-string is bigger then *max-num-description-columns*,
then description is inserted newline for easy to see.
Default value is 80.")

(defparameter *description-indent-num* 6)

(defun pprint-description (desc)
  (let ((len (length desc))
        (max-nchars (- *max-num-description-columns*
                       *description-indent-num*)))
    (if (<= len max-nchars)
        (format t "~%      ~A" desc)
        (let ((space-index
                (loop :for i :downfrom max-nchars :to 0
                      :until (char= #\Space (char desc i))
                      :finally (return (1+ i)))))
          (if (zerop space-index)
              (progn
                (format t "~%      ~A-" (subseq desc 0 (1- max-nchars)))
                (pprint-description (subseq desc (1- max-nchars))))
              (progn
                (format t "~%      ~A" (subseq desc 0 (1- space-index)))
                (pprint-description (subseq desc space-index))))))))

(defun strip (string)
  (string-trim '(#\Space #\Return #\Newline) string))

(defun remove-tags (string)
  (ppcre:regex-replace-all "(<.+?>)" string ""))

(defun gen-query (format-string word-string)
  (format nil format-string
          (nsubstitute #\+ #\Space word-string :test #'char=)))


;;--------------------------------------------------------------------
;; Cliki search
;;--------------------------------------------------------------------
(defun search-cliki (word-string)
  (let* ((query    (gen-cliki-query word-string))
         (response (drakma:http-request query))
         (results  (extract-cliki-results response)))
    (when results
      (format t "~% SEARCH-SPACE: Cliki~%")
      (print-cliki-results results)
      (awhen (extract-cliki-next-page-url response)
        (loop :for url :in it
              :for res := (drakma:http-request url)
              :do (print-cliki-results (extract-cliki-results res))))
      (terpri)
      t)))

(defun gen-cliki-query (word-string)
  (gen-query "http://www.cliki.net/site/search?query=~A" word-string))

(defun extract-cliki-results (response)
  (let* ((results (ppcre:scan-to-strings
                   "(?s)<ol start=.+?>(.+?)</ol>" response))
         (repos (ppcre:all-matches-as-strings
                 "(?s)<li>(.+?)</li>" results)))
    (when repos
      (iter (for repo :in repos)
            (ppcre:register-groups-bind (url title description)
                ("(?s)<li><a href=\"(.+?)\" class=\"internal\">(.+?)</a>\\s?<br\\s?/?>(.+?)</li>"
                 repo)
              (collect (list title
                             (when *url-print-p* url)
                             (when *description-print-p*
                               (strip (remove-tags description))))))))))

(defun print-cliki-results (results)
  (loop :for (title url desc) :in results :do
     (format t "~&  ~A" (html-entities:decode-entities title))
     (when *url-print-p*
       (format t "~%      http://www.cliki.net~A" url))
     (when *description-print-p*
       (pprint-description
        (html-entities:decode-entities desc)))))

(defun extract-cliki-next-page-url (response)
  (let ((urls nil)
        (paginator (ppcre:scan-to-strings
                    "(?s)<div id=\"paginator\">(.+?)</div>" response)))
    (ppcre:do-register-groups (query)
        ("<a href=\"\\\?query=(.+?)\">" paginator)
      (push (gen-cliki-query query) urls))
    (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
      (subseq rest-urls
              0 (min (max-num-next-pages) (length rest-urls))))))


;;--------------------------------------------------------------------
;; GitHub search
;;--------------------------------------------------------------------
;; 2013-03-10
;; Changed: github-search api-v3 legacy search -> advanced search.
;;          For github api-v3, search option '?language=' does not work??.

;; drakma's default url-encoder does not support %encoding.
;; e.g. in url, want "Common Lisp" -> "Common%20Lisp"
;;              but  "Common Lisp" -> error
;;                   "Common+Lisp" -> "Common%2BLisp"
;;                   "Common%20Lisp" -> "Common%2520Lisp"
;; so, (do-urlencode:urlencode search-word) in gen-github-query
;;     (drakma:http-request query :preserve-uri t) in search-github
;; (defun gen-github-query (search-word)
;;   (format nil "https://api.github.com/legacy/repos/search/~A~
;;                ?language=Common%20Lisp"
;;           (do-urlencode:urlencode search-word)))

;; !! TODO !! yason: hash-table -> alist
;; (defun search-github (search-word)
;;   (let* ((query (gen-github-query search-word))
;;          (res   (drakma:http-request query :preserve-uri t))
;;          (jason (yason:parse (flexi-streams:octets-to-string
;;                               res :external-format :utf-8)))
;;          (repos (gethash "repositories" jason)))
;;     (when repos
;;       (format t "~% SEARCH-SPACE: GitHub-Repos~%")
;;       (dolist (repo repos)
;;         (unless (gethash "fork" repo)   ;only master is displayed
;;           (format t "~&  ~A" (gethash "name" repo))
;;           (when *output-url-p*
;;             (format t "~%      https://github.com/~A/~A"
;;                      (gethash "username" repo) (gethash "name" repo)))
;;           (awhen (and *output-description-p*
;;                       (gethash "description" repo))
;;             (unless (string= "" it)
;;               (pprint-description it)))))
;;       (terpri)
;;       t)))


(defun search-github (word-string)
  (let* ((query    (gen-github-query word-string))
         (response (drakma:http-request query :preserve-uri t))
         (results  (extract-github-results response)))
    (when results
      (format t "~% SEARCH-SPACE: GitHub-Repos~%")
      (print-github-results results)
      (awhen (extract-github-next-page-url response)
        (loop :for url :in it
              :for res := (drakma:http-request url :preserve-uri t)
              :do (print-github-results
                   (extract-github-results res))))
      (terpri)
      t)))

(defun gen-github-query (word-string)
  (gen-query "https://github.com/search?q=~A~
              &type=Repositories&ref=advsearch&l=Common+Lisp"
             word-string))

(defun extract-github-results (response)
  (let* ((results (ppcre:scan-to-strings
                   "(?s)<ul class=\"repolist js-repo-list\">(.+<!-- /.body -->)"
                   response))
         (repos (ppcre:all-matches-as-strings
                 "(?s)<h3>(.+?)</p>"
                 results)))
    (when repos
      (iter (for repo :in repos)
            (ppcre:register-groups-bind (url title)
                ("(?s)<h3>.+?<a href=\"/(.+?)\">.+?/(.+?)</a>" repo)
              (collect
                  (list (ppcre:regex-replace-all "</?em>" title "")
                        (when *url-print-p* url)
                        (when *description-print-p*
                          (ppcre:register-groups-bind
                              (description)
                              ("(?s)<p class=\"description\">(.+?)</p>" repo)
                            (strip (remove-tags description)))))))))))

(defun print-github-results (results)
  (loop :for (title url desc) :in results :do
     (format t "~&  ~A" (html-entities:decode-entities title))
     (when *url-print-p*
       (format t "~%      https://github.com/~A" url))
     (when (and *description-print-p* desc)
       (pprint-description
        (html-entities:decode-entities desc)))))

(defun extract-github-next-page-url (response)
  (let ((urls nil)
        (pagination
         (ppcre:scan-to-strings
          "(?s)<div class=\"pagination\"(.+?)</div>" response)))
    (ppcre:do-register-groups (next-url)
        ("<a href=\"(.+?)\"( (class|rel).+?)?>" pagination)
      (push (format nil "https://github.com~A"
                    (html-entities:decode-entities next-url))
            urls))
    (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
      (subseq rest-urls
              0 (min (max-num-next-pages) (length rest-urls))))))


;;--------------------------------------------------------------------
;; BitBucket search
;;--------------------------------------------------------------------
(defun search-bitbucket (word-string)
  (let* ((query    (gen-bitbucket-query word-string))
         (response (drakma:http-request query :preserve-uri t))
         (results  (extract-bitbucket-results response)))
    (when results
      (format t "~% SEARCH-SPACE: BitBucket-Repos~%")
      (print-bitbucket-results results)
      (awhen (extract-bitbucket-next-page-url response)
        (loop :for url :in it
              :for res := (drakma:http-request url)
              :do (print-bitbucket-results
                   (extract-bitbucket-results res))))
      (terpri)
      t)))

(defun gen-bitbucket-query (word-string)
  (gen-query "https://bitbucket.org/repo/all/relevance?name=~A~
              &language=common+lisp"
             word-string))

(defun extract-bitbucket-results (response)
  (let* ((results (ppcre:scan-to-strings
                   "(?s)<section id=\"repo-list\">(.+?)</section>"
                   response))
         (repos (ppcre:all-matches-as-strings
                 "(?s)<article class=\"repo-summary\">(.+?)</article>"
                 results)))
    (when repos
      (iter (for repo :in repos)
            (ppcre:register-groups-bind (url title)
                ("(?s)<a class=\"repo-link\" href=\"(.+?)\">.+? / (.+?)</a>"
                 repo)
              (collect (list title
                             (when *url-print-p* url)
                             (when *description-print-p*
                               (ppcre:register-groups-bind (description)
                                   ("(?s)<p>(.+?)</p>" repo)
                                 (strip (remove-tags description)))))))))))

(defun print-bitbucket-results (results)
  (loop :for (title url desc) :in results :do
     (format t "~&  ~A" (html-entities:decode-entities title))
     (when *url-print-p*
       (format t "~%      https://bitbucket.org~A" url))
     (when (and *description-print-p* desc)
       (pprint-description
        (html-entities:decode-entities desc)))))

(defun extract-bitbucket-next-page-url (response)
  (let ((urls nil)
        (paginator (ppcre:scan-to-strings
                    "(?s)<ol class=\"paginator\">(.+?)</ol>" response)))
    (ppcre:do-register-groups (next-url)
        ("<a href=\"(.+?)\">" paginator)
      (push next-url urls))
    (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
      (subseq rest-urls
              0 (min (max-num-next-pages) (length rest-urls))))))


;;====================================================================
