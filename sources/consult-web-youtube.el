;;; consult-web-youtube.el --- Consulting YouTube -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-web)

(defun consult-web-dynamic--youtube-format-candidate (table &optional face &rest args)
"Formats a candidate for `consult-web-youtube' commands.

TABLE is a hashtable with metadata for the candidate as (key value) pairs.
Returns a string (from :title field in TABLE)
with text-properties that conatin
all the key value pairs in the table.
"
  (let* ((pl (consult-web-hashtable-to-plist table))
         (title (format "%s" (gethash :title table)))
         (url (gethash :url table))
         (urlobj (if url (url-generic-parse-url url)))
         (domain (if (url-p urlobj) (url-domain urlobj)))
         (domain (if (stringp domain) (propertize domain 'face 'consult-web-domain-face)))
         (channeltitle (gethash :channeltitle table))
         (channeltitle (if (stringp channeltitle) (propertize channeltitle 'face 'consult-web-path-face)))
         (source (gethash :source table))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face)))
         (query (gethash :query table))
         (snippet (gethash :snippet table))
         (snippet (if (and snippet (stringp snippet) (> (string-width snippet) 25)) (concat (substring snippet 0 22) "...") snippet))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or face 'consult-web-default-face)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :search-url :query :source :snippet :channeltitle :videoid)))
         (str (concat title-str
                      (if domain (format "\t%s" domain))
                      (if channeltitle (format " - %s" channeltitle))
                      (if snippet (format "\s\s%s" snippet))
                      (if source (concat "\t" source))
                      (if extra-args (propertize (format "\s%s" extra-args) 'face 'consult-web-source-face))))
         (str (apply #'propertize str pl))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defvar consult-web-youtube-watch-url "https://www.youtube.com/watch")

(defvar consult-web-youtube-channel-url "https://www.youtube.com/channel/")

(defvar consult-web-youtube-search-results-url "https://www.youtube.com/results")

(defvar consult-web-youtube-search-api-url "https://www.googleapis.com/youtube/v3/search")

(defcustom consult-web-youtube-search-key nil
"Key for YouTube custom search API

See URL `https://developers.google.com/youtube/v3/getting-started'
for details"
:group 'consult-web
:type '(choice (const :tag "API Key" string)
               (function :tag "Custom Function")))


(cl-defun consult-web--youtube-fetch-results (input &rest args &key count page order def type vidtype &allow-other-keys)
  "Fetches search results for INPUT from “Google custom search” service.

COUNT is passed as num in query parameters.
(* PAGE COUNT) is passed as start in query paramters.
"
  (let* ((count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                     (and page (string-to-number (format "%s" page)))
                     consult-web-default-count))
         (def (if (and def (member (format "%s" def) '("any" "standard" "high"))) (format "%s" def) "any"))
         (type (if (and type (member (format "%s" type) '("channel" "playlist" "video"))) (format "%s" type) "video"))
         (vidtype (if (and type (member (format "%s" vidtype) '("any" "episode" "movie"))) (format "%s" vidtype) "any"))
         (count (min count 10))
         (page (+ (* page count) 1))
         (order  (if (and type (member (format "%s" order) '("date" "rating" "relevance" "title" "videoCount" "viewCount"))) (format "%s" vidtype) "relevance"))
         (params `(("q" . ,input)
                   ("part" . "snippet")
                   ("order" . ,order)
                   ("type" . ,type)
                   ("maxResults" . ,(format "%s" count))
                   ("videoDefinition" . ,def)
                   ("videoType" . ,vidtype)))
         (headers `(("Accept" . "application/json")
                    ("Accept-Encoding" . "gzip")
                    ("User-Agent" . "consult-web (gzip)")
                    ("X-Goog-Api-Key" . ,(consult-web-expand-variable-function consult-web-google-customsearch-key)))))
    (funcall consult-web-retrieve-backend
     consult-web-youtube-search-api-url
     :params params
     :headers headers
     :parser
     (lambda ()
       (goto-char (point-min))
       (let* ((results (json-parse-buffer))
              (items (gethash "items" results)))
         (cl-loop for a across items
                  collect
                  (let* ((table (make-hash-table :test 'equal))
                         (videoid (gethash "videoId" (gethash "id" a)))
                         (snippet (gethash "snippet" a))
                         (channeltitle (gethash "channelTitle" snippet))
                         (channelid (gethash "channelId" snippet))
                         (title (gethash "title" snippet))
                         (date (gethash "publishedAt" snippet))
                         (date (format-time-string "%Y-%m-%d %R" (date-to-time date)))
                         (url (cond
                               (videoid (consult-web--make-url-string consult-web-youtube-watch-url `(("v" . ,videoid))))
                               (channelid (concat consult-web-youtube-channel-url channelid))))
                         (search-url (consult-web--make-url-string consult-web-youtube-search-results-url `(("search_query" . ,input))))
                         (description (gethash "description" snippet)))
                    (puthash :url
                             url table)
                    (puthash :search-url search-url
                             table)
                    (puthash :title title
                             table)
                    (puthash :videoid videoid
                             table)
                    (puthash :channeltitle channeltitle
                             table)
                    (puthash :channelid channelid
                             table)
                    (puthash :source "YouTube"
                             table)
                    (puthash :query input
                             table)
                    (puthash :snippet description table)
                    table
                    )
                  ))
))))

(consult-web-define-source "YouTube"
                           :narrow-char ?y
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--youtube-fetch-results
                           :format #'consult-web-dynamic--youtube-format-candidate
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-youtube' module

(provide 'consult-web-youtube)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-youtube)
;;; consult-web-youtube.el ends here
