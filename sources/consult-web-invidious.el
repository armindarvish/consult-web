;;; consult-web-invidious.el --- Consulting Invidious -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (consult-web "0.2") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-web)

(defvar consult-web-invidious-servers nil)
(defvar consult-web-invidious-server-url "https://api.invidious.io/instances.json")

(cl-defun consult-web--invidious-format-candidate (&rest args &key source type query title snippet channeltitle date subcount videocount viewcount length face &allow-other-keys)
"Formats a candidate for `consult-web-invidious' commands.

SOURCE is the name to use (e.g. “Invidious”)

TYPE is the type of candidate (e.g. video, channel, playlist)

QUERY is the query input from the user

TITLE is the title of the video

SNIPPET is a string containing a snippet/description of the video

CHANNELTITLE is the name of the channel for the video

DATE is the publish date of the video

SUBCOUNT is the subscriber count fpr a channel

VIDEOCOUNT is the number of videos in a playlist

VIEWCOUNT is the number of times a video is viewed

LENGTH is the duration of a  video in seconds

FACE is the face to apply to TITLE
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (propertize source 'face 'consult-web-source-face))
         (match-str (if (stringp query) (consult--split-escaped query) nil))
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
                  (and (numberp length) (seconds-to-string length))
                  (and (equal type "playlist") "[PLAYLIST]")
                  (and (equal type "channel") "(CHANNEL)")))
         (length (and (stringp length) (consult-web--set-string-width (propertize length 'face 'consult-web-comment-face) 10)))
         (date (propertize (or (and (stringp date) date) (make-string 10 ?\s)) 'face 'consult-web-date-face))
         (channeltitle (and channeltitle (stringp channeltitle) (propertize channeltitle 'face 'consult-web-path-face)))
         (channeltitle (consult-web--set-string-width channeltitle (* 2 frame-width-percent)))
         (snippet (if (stringp snippet) (consult-web--set-string-width (replace-regexp-in-string "\n" "  " snippet) (* 2 frame-width-percent))))
         (snippet (and snippet (stringp snippet) (propertize snippet 'face 'consult-web-snippet-face)))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-default-face))
         (title-str (and title (propertize title 'face face)))
         (title-str (consult-web--set-string-width title-str (* 5 frame-width-percent)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when channeltitle (concat " " channeltitle))
                      (propertize " " 'display `(space :align-to ,(+ (* 5 frame-width-percent)                                                                      11)))
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

(defun consult-web--invidious-get-servers (&optional rotate)
  "Get list of Invidious API servers.
"
  (when (and consult-web-invidious-servers rotate)
    (setq consult-web-invidious-servers
          (nconc (cdr consult-web-invidious-servers)
                 (list (car consult-web-invidious-servers)))))
  (or consult-web-invidious-servers
      (setq consult-web-invidious-servers
            (let ((params `(("pretty" . "1")
                                  ("sort_by" . "type"))))
       (consult-web--fetch-url
        consult-web-invidious-server-url
        consult-web-http-retrieve-backend
        :params params
        :sync t
        :parser #'consult-web--json-parse-buffer
        :callback (lambda (attrs)
  (delq nil (mapcar (lambda (item)
                      (when (equal (gethash "api" (cadr item)) t)
                                  (gethash "uri" (cadr item))
                      )

                      ) attrs))
))))))

(cl-defun consult-web--invidious-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results for INPUT from “Invidious” service."
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (type (plist-get opts :type))
               (order (or (plist-get opts :order) (plist-get opts :sort)))
               (channel (or (plist-get opts :channel) (plist-get opts :user)))
               (subs (or (plist-get opts :subs) (plist-get opts :subscriptions)))
               (searchdate (plist-get opts :date))
               (searchdate (if searchdate (format "%s" searchdate)))
               (features (plist-get opts :features))
               (features (if features (format "%s" features)))
               (duration (plist-get opts :dur))
               (duration (if duration (format "%s" duration)))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-web-default-count))
               (count (min count 10))
               (page (+ (* page count) 1))
               (order  (if (and order (member (format "%s" order) '("date" "rating" "relevance" "upload_date" "views" "view_count"))) (format "%s" order) "relevance"))
               (type (if (and type (member (format "%s" type) '("channel" "playlist" "video" "movie" "show" "all"))) (format "%s" type) "video"))
               (params (delq nil `(("q" . ,(replace-regexp-in-string " " "+" query))
                         ("sort_by" . ,order)
                         ("type" . ,type)
                         ,(when searchdate `("date" . ,searchdate))
                         ,(when features `("features" . ,features))
                         ,(when duration `("duration" . ,duration))
                         ,(when subs `("subscriptions" . ,(if subs "true" "false"))))))
               (server-url (car (consult-web--invidious-get-servers)))
               (api-url (concat server-url "/api/v1/search?")))
    (consult-web--fetch-url api-url consult-web-http-retrieve-backend
                            :encoding 'utf-8
                            :params params
                            :parser #'consult-web--json-parse-buffer
                            :callback
                            (lambda (attrs)
                              (let* ((raw-results attrs)
                                     (annotated-results
                                      (mapcar (lambda (item)
                                                (let*
                                                    ((source "Invidious")
                                                     (item-type (gethash "type" item))
                                                     (channelhandle (gethash "channelHandle" item))
                                                     (title (or (gethash "title" item)
                                                                (unless (eq channelhandle :null) channelhandle)
                                                                (gethash "author" item)
                                                                ))
                                                     (videos  (gethash "videos" item))
                                                     (videoid (or (gethash "videoId" item)
                                                                  (and videos (gethash "videoId" (car videos)))))
                                                     (channeltitle (gethash "author" item))
                                                     (channelid (gethash "authorId" item))
                                                     (playlistid (gethash "playlistId" item))
                                                     (videocount (gethash "videoCount" item))
                                                     (subcount (gethash "subCount" item))
                                                     (viewcount (gethash "viewCount" item))
                                                     (videolength (gethash "lengthSeconds" item))
                                                     (date (gethash "published" item))
                                                     (date (when date (format-time-string "%Y-%m-%d" (seconds-to-time date))))
                                                     (url (cond
                                                           ((and playlistid videoid) (consult-web--make-url-string consult-web-youtube-watch-url `(("v" . ,videoid)
                               ("list" . ,playlistid))))
                                                           (playlistid (consult-web--make-url-string consult-web-youtube-watch-url `(("list" . ,playlistid))))
                                                           (videoid (consult-web--make-url-string consult-web-youtube-watch-url `(("v" . ,videoid))))

                                                           (channelid (concat consult-web-youtube-channel-url channelid))))
                                                     (search-url (consult-web--make-url-string server-url params))
                                                     (description (gethash "description" item))

                                                     (decorated (consult-web--invidious-format-candidate :source source :type item-type :query query :title title :snippet description :channeltitle channeltitle :date date :subcount subcount :videocount videocount :viewcount viewcount :length videolength)))
                                                (propertize decorated
                                                            :source source
                                                            :title title
                                                            :url url
                                                            :search-url search-url
                                                            :query query
                                                            :snippet description
                                                            :videoid videoid
                                                            :channeltitle channeltitle
                                                            :channelid channelid
                                                            :duration duration
:views viewcount
:videocount videocount
:subscriptions subcount)))

                                      raw-results)))
                              (when annotated-results
                                (funcall callback annotated-results))
                              annotated-results)
                              ))))

(consult-web-define-source "Invidious"
                           :narrow-char ?I
                           :type 'dynamic
                           :category 'consult-web-video
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--invidious-fetch-results
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (bound-and-true-p consult-web-invidious-server-url))
                           :group #'consult-web--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-web-invidious' module

(provide 'consult-web-invidious)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-invidious)
;;; consult-web-invidious.el ends here
