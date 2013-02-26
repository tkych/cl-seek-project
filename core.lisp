;;;; Last modified : 2013-02-26 19:40:28 tkych

;; cl-seek-project/core.lisp


;;====================================================================
;; Core
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-seek-project
  (:nicknames #:seek-project)
  (:use :cl :iterate)
  (:import-from #:anaphora #:aif #:awhen #:it)
  (:export #:seek #:*description-max-num-cols*))

(in-package #:cl-seek-project)


;;--------------------------------------------------------------------
;; Seek
;;--------------------------------------------------------------------
(defparameter *output-description-p* t)
(defparameter *output-url-p* t)

;; !! TODO !! add: search-space google-code
;; !! TODO !! limit output num:
;;            *display-max-num-projects*, if over limit, y-or-n?
(defun seek (search-word &key (web? t) (description? nil) (url? nil)
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
    GitHub    - 100
    Cliki     - 50
    BitBucket - 50
"
  (unless (or (stringp search-word) (symbolp search-word))
    (error "~S is not string or symbol." search-word))
  (unless (or quicklisp?
              (and web? (or cliki? github?)))
    (error "There is no search-space."))
  (let ((found? nil)
        (*output-url-p* url?)
        (word-string
         (write-to-string search-word :case :downcase :escape nil)))
    (format t "~&SEARCH-RESULT: ~S~%" word-string)

    #+quicklisp  ;!? quicklisp is not library, so probably need add this !?
    (when (and quicklisp? (search-quicklisp word-string))
      (setf found? t))

    (when web?
      (let ((drakma:*drakma-default-external-format* :utf-8)
            (*output-description-p* description?))
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
#+quicklisp  ;!? quicklisp is not library, so probably need add this !?
(progn
  (defun search-quicklisp (word-string)
    (let ((found? nil))
      (dolist (wd (ppcre:split " " word-string))
        (awhen (search-ql-systems wd)
          (unless found?
            (format t "~% SEARCH-SPACE: Quicklisp~%")
            (setf found? t))
          (output-ql-results it)))
      found?))

  (defun search-ql-systems (word-string)
    (loop :for system :in (ql-dist:provided-systems t)
          :when (or (search word-string (ql-dist:name system))
                    (search word-string
                            (ql-dist:name (ql-dist:release system))))
          :collect system))

  (defun output-ql-results (systems)
    (dolist (system systems)
      (format t "~&  ~A" (ql-dist:name system))
      ;; (when *output-url-p*
      ;;   (format t "~%      ~A"
      ;;           ))
      ;; (when *output-description-p*
      ;;   (format t "~%      ~A"
      ;;           (ql-dist:short-description system)))
      (terpri)))

  ) ;end of #+quicklisp


;;--------------------------------------------------------------------
;; web search common functions
;;--------------------------------------------------------------------
(defparameter *description-max-num-cols* 80
  "If the length of description-string is bigger then *description-max-num-cols*, then search-result is inserted newline for easy to see.
Default value is 80.")

(defparameter *description-indent-num* 6)

(defun pprint-description (desc)
  (let ((len (length desc))
        (max-nchars (- *description-max-num-cols*
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


;;--------------------------------------------------------------------
;; Cliki search
;;--------------------------------------------------------------------
(defun gen-cliki-query (word-string)
  (format nil "http://www.cliki.net/site/search?query=~A"
          (ppcre:regex-replace-all " " word-string "+")))

(defun search-cliki (word-string)
  (let* ((query    (gen-cliki-query word-string))
         (response (drakma:http-request query))
         (results  (extract-cliki-results response)))
    (when results
      (format t "~% SEARCH-SPACE: Cliki~%")
      (output-cliki-results results)
      (awhen (extract-cliki-rest-page-url response)
        (loop :for url :in it
              :for res := (drakma:http-request url)
              :do (output-cliki-results (extract-cliki-results res))))
      (terpri)
      t)))

(defun extract-cliki-results (response)
  (let ((<li>s (ppcre:all-matches-as-strings "(?s)<li>(.+?)</li>"
                 (ppcre:scan-to-strings
                  "(?s)<ol start=.+?>(.+?)</ol>" response))))
    (when <li>s
      (iter (for <li> :in <li>s)
            (ppcre:register-groups-bind (url title description)
                ("(?s)<li><a href=\"(.+?)\" class=\"internal\">(.+?)</a>\\s?<br\\s?/?>(.+?)</li>"
                 <li>)
              (collect (list title
                             (when *output-url-p* url)
                             (when *output-description-p*
                               (strip (remove-tags description))))))))))

(defun output-cliki-results (results)
  (loop :for (title url description) :in results
        :do (format t "~&  ~A" (html-entities:decode-entities title))
            (when *output-url-p*
              (format t "~%      http://www.cliki.net~A" url))
            (when *output-description-p*
              (pprint-description
               (html-entities:decode-entities description)))))

(defparameter *cliki-max-num-of-result-pages* 4) ;total 50

;; extract-cliki-rest-page-url
(defun extract-cliki-rest-page-url (response)
  (let ((urls nil)
        (paginator (ppcre:scan-to-strings
                    "(?s)<div id=\"paginator\">(.+?)</div>" response)))
    (ppcre:do-register-groups (query)
        ("<a href=\"\\\?query=(.+?)\">" paginator)
      (push (gen-cliki-query query) urls))
    (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
      (subseq rest-urls
              0 (min *cliki-max-num-of-result-pages*
                     (length rest-urls))))))


;;--------------------------------------------------------------------
;; GitHub search
;;--------------------------------------------------------------------
;; drakma's default url-encoder does not support %encoding.
;; e.g. in url, want "Common Lisp" -> "Common%20Lisp"
;;              but  "Common Lisp" -> error
;;                   "Common+Lisp" -> "Common%2BLisp"
;;                   "Common%20Lisp" -> "Common%2520Lisp"
;; so, (do-urlencode:urlencode search-word) in gen-github-query
;;     (drakma:http-request query :preserve-uri t) in search-github

(defun gen-github-query (search-word)
  (format nil "https://api.github.com/legacy/repos/search/~A~
               ?language=Common%20Lisp"
          (do-urlencode:urlencode search-word)))

;; !! TODO !! max num result is 100. <- github api-v3
;;            How to know there exists results more than 100?
(defun search-github (search-word)
  (let* ((query (gen-github-query search-word))
         (res   (drakma:http-request query :preserve-uri t))
         (jason (yason:parse (flexi-streams:octets-to-string
                              res :external-format :utf-8)))
         (repos (gethash "repositories" jason)))
    (when repos
      (format t "~% SEARCH-SPACE: GitHub-Repos~%")
      (dolist (repo repos)
        (unless (gethash "fork" repo)   ;only master is displayed
          (format t "~&  ~A" (gethash "name" repo))
          (when *output-url-p*
            (format t "~%      https://github.com/~A/~A"
                     (gethash "username" repo) (gethash "name" repo)))
          (awhen (and *output-description-p*
                      (gethash "description" repo))
            (pprint-description it))))
      (terpri)
      t)))


;;--------------------------------------------------------------------
;; BitBucket search
;;--------------------------------------------------------------------
(defun search-bitbucket (word-string)
  (let* ((query    (gen-bitbucket-query word-string))
         (response (drakma:http-request query :preserve-uri t))
         (results  (extract-bitbucket-results response)))
    (when results
      (format t "~% SEARCH-SPACE: BitBucket-Repos~%")
      (output-bitbucket-results results)
      (awhen (extract-bitbucket-rest-page-url response)
        (loop :for url :in it
              :for res := (drakma:http-request url)
              :do (output-bitbucket-results
                   (extract-bitbucket-results res))))
      (terpri)
      t)))

(defun gen-bitbucket-query (word-string)
  (format nil "https://bitbucket.org/repo/all/relevance?name=~A&language=common+lisp"
          (ppcre:regex-replace-all " " word-string "+")))

(defun extract-bitbucket-results (response)
  (let ((<article>s (ppcre:all-matches-as-strings
                     "(?s)<article class=\"repo-summary\">(.+?)</article>"
                     (ppcre:scan-to-strings
                      "(?s)<section id=\"repo-list\">(.+?)</section>"
                      response))))
    (when <article>s
      (iter (for <article> :in <article>s)
            (ppcre:register-groups-bind (url title)
                ("(?s)<a class=\"repo-link\" href=\"(.+?)\">.+? / (.+?)</a>"
                 <article>)
              (collect (list title
                             (when *output-url-p* url)
                             (when *output-description-p*
                               (or (ppcre:register-groups-bind (description)
                                       ("(?s)<p>(.+?)</p>" <article>)
                                     (strip (remove-tags description)))
                                   "")))))))))

(defun output-bitbucket-results (results)
  (loop :for (title url description) :in results
        :do (format t "~&  ~A" (html-entities:decode-entities title))
            (when *output-url-p*
              (format t "~%      https://bitbucket.org~A" url))
            (when *output-description-p*
              (pprint-description
               (html-entities:decode-entities description)))))

(defparameter *bitbucket-max-num-of-result-pages* 4) ;total 50

(defun extract-bitbucket-rest-page-url (response)
  (let ((urls nil)
        (paginator (ppcre:scan-to-strings
                    "(?s)<ol class=\"paginator\">(.+?)</ol>" response)))
    (ppcre:do-register-groups (next-url)
        ("<a href=\"(.+?)\">" paginator)
      (push next-url urls))
    (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
      (subseq rest-urls
              0 (min *bitbucket-max-num-of-result-pages*
                     (length rest-urls))))))


;;====================================================================
