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

(cl-defun consult-web--youtube-format-candidate (&rest args &key source type query title snippet channeltitle date length subcount videocount viewcount face &allow-other-keys)
"Formats a candidate for `consult-web-youtube' commands.

SOURCE is the name to use (e.g. “YouTube”)

QUERY is the query input from the user

TITLE is the title of the video

SNIPPET is a string containing a snippet/description of the video

DATE is the publish date of the video

CHANNELTITLE is the name of the channel for the video

FACE is the face to apply to TITLE
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (propertize source 'face 'consult-web-source-face))
         (match-str (if (stringp query) (consult--split-escaped query) nil))
         (date (and (stringp date) (propertize date 'face 'consult-web-date-face)))
         (channeltitle (and (stringp channeltitle) (propertize channeltitle 'face 'consult-web-path-face)))
         (channeltitle (consult-web--set-string-width channeltitle (* 2 frame-width-percent)))
         (snippet (if (stringp snippet) (consult-web--set-string-width snippet (* 2 frame-width-percent))))
         (snippet (and snippet (stringp snippet) (propertize snippet 'face 'consult-web-snippet-face)))
         (videocount-str (and videocount (consult-web--numbers-human-readable (or videocount 0) "videos")))
         (viewcount-str (and viewcount (consult-web--numbers-human-readable (or viewcount 0) "views")))
         (subcount-str (and subcount (consult-web--numbers-human-readable (or subcount 0) "subs")))
         (stats (and type
                    (stringp type)
                    (propertize
                     (consult-web--set-string-width (pcase type
                      ("video" (format "%s" (or viewcount-str "0 views")))
                      ("playlist" (format "%s" (or videocount-str "0 videos")))
                      ("channel" (format "%s" (or subcount-str "0 subscriptions")))
                      (_ "")) 10)
                     'face 'consult-web-domain-face)))
         (length (or
                  (and (numberp length) length)
                  (and (equal type "playlist") "[PLAYLIST]")
                  (and (equal type "channel") "(CHANNEL)")))
         (length (and (stringp length) (consult-web--set-string-width (propertize length 'face 'consult-web-comment-face) 10)))

         (face (or (consult-web--get-source-prop source :face) face 'consult-web-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 5 frame-width-percent)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when channeltitle (concat " " channeltitle))
                      (when length (concat "\s" length))
                      (unless (string-empty-p stats) (concat "\s" stats))
                      (when snippet (concat "\s\s" snippet))
                      (concat "\t" source)))
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
  "Key for “YouTube Data API”

See URL `https://developers.google.com/youtube/v3/getting-started'
for details"
  :group 'consult-web
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(cl-defun consult-web--youtube-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results for INPUT from “YouTube Data API” service.
"
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (def (plist-get opts :def))
               (type (plist-get opts :type))
               (vidtype (plist-get opts :vidtype))
               (order (or (plist-get opts :order) (plist-get opts :sort)))
               (count (or (and (integerp count) count)
                    (and count (string-to-number (format "%s" count)))
                    consult-web-default-count))
         (page (or (and (integerp page) page)
                   (and page (string-to-number (format "%s" page)))
                   consult-web-default-count))
               (def (if (and def (member (format "%s" def) '("any" "standard" "high"))) (format "%s" def) "any"))
               (type (if (and type (member (format "%s" type) '("channel" "playlist" "video"))) (format "%s" type) "video"))
               (vidtype (if (and vidtype (member (format "%s" vidtype) '("any" "episode" "movie"))) (format "%s" vidtype) "any"))
               (count (min count 10))
               (page (+ (* page count) 1))
               (order  (if (and order (member (format "%s" order) '("date" "rating" "relevance" "title" "videoCount" "viewCount"))) (format "%s" order) "relevance"))
               (params `(("q" . ,(replace-regexp-in-string " " "+" query))
                         ("part" . "snippet")
                         ("order" . ,order)
                         ("type" . ,type)
                         ("maxResults" . ,(format "%s" count))
                         ("videoDefinition" . ,def)
                         ("videoType" . ,vidtype)
                         ("key" . ,(consult-web-expand-variable-function consult-web-youtube-search-key))
                         ))
               (headers `(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("User-Agent" . "consult-web (gzip)")
                          ("X-Goog-Api-Key" . ,(consult-web-expand-variable-function consult-web-youtube-search-key)))))
    (consult-web--fetch-url consult-web-youtube-search-api-url consult-web-http-retrieve-backend
                            :encoding 'utf-8
                            :params params
                            :headers headers
                            :parser #'consult-web--json-parse-buffer
                            :callback
                            (lambda (attrs)
                              (let* ((raw-results (gethash "items" attrs))
                                     (annotated-results
                                      (mapcar (lambda (item)
                                                (let*
                                                    ((source "YouTube")
                                                     (videoid (gethash "videoId" (gethash "id" item)))
                                                     (snippet (gethash "snippet" item))
                                                     (channeltitle (gethash "channelTitle" snippet))
                                                     (channelid (gethash "channelId" snippet))
                                                     (title (gethash "title" snippet))
                                                     (date (gethash "publishedAt" snippet))
                                                     (date (format-time-string "%Y-%m-%d" (date-to-time date)))
                                                     (url (cond
                                                           (videoid (consult-web--make-url-string consult-web-youtube-watch-url `(("v" . ,videoid))))
                                                           (channelid (concat consult-web-youtube-channel-url channelid))))
                                                     (search-url (consult-web--make-url-string consult-web-youtube-search-results-url `(("search_query" . ,query))))
                                                     (description (gethash "description" snippet))

                                                     (decorated (consult-web--youtube-format-candidate :source source :query query :title title :snippet description :channeltitle channeltitle :date date)))
                                                (propertize decorated
                                                            :source source
                                                            :title title
                                                            :url url
                                                            :search-url search-url
                                                            :query query
                                                            :snippet description
                                                            :videoid videoid
                                                            :channeltitle channeltitle
                                                            :channelid channelid)))

                                      raw-results)))
                              (when annotated-results
                                (funcall callback annotated-results))
                              annotated-results)))))

(consult-web-define-source "YouTube"
                           :narrow-char ?y
                           :type 'async
                           :category 'consult-web-video
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--youtube-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (bound-and-true-p consult-web-youtube-search-key))
                           :group #'consult-web--group-function
                           :sort t
                           :dynamic 'both
                           :annotate nil
                           )

;;; provide `consult-web-youtube' module

(provide 'consult-web-youtube)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-youtube)
;;; consult-web-youtube.el ends here
