;;; consult-web-elfeed.el --- Consulting Elfeed -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-web/blob/main/consult-web-sources
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'elfeed nil t)
(require 'consult-web)

;;; Customization Variables
(defcustom consult-web-elfeed-search-buffer-name "*consult-web-elfeed-search*"
  "Name for consult-web-elfeed-search buffer."
  :type 'string)

(defcustom consult-web-elfeed-default-filter nil
  "Default Filter for consult-web-elfeed-search."
  :type 'string)

(defun consult-web--elfeed-format-candidate (entries query)
"Formats the cnaiddates of `consult-web-elfeed'.

ENTRIES are entries from `consult-web--elfeed-fetch-result'.
QUERY is the query input from the user"
  (let ((annotated-entries))
    (dolist (entry entries annotated-entries)
      (let* ((url (elfeed-entry-link entry))
             (urlobj (if url (url-generic-parse-url url)))
             (domain (if (url-p urlobj) (url-domain urlobj)))
             (domain (if (stringp domain) (propertize domain 'face 'consult-web-domain-face)))
             (path (if (url-p urlobj) (url-filename urlobj)))
             (path (if (stringp path) (propertize path 'face 'consult-web-path-face)))
             (title (or (elfeed-entry-title entry) ""))
             (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
             (feed (elfeed-entry-feed entry))
             (feed-title (when feed (elfeed-feed-title feed)))
             (date (format-time-string "%Y-%m-%d %H:%M" (elfeed-entry-date entry)))
             (id (elfeed-entry-id entry))
             (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
             (tags-str (mapconcat
                        (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                        tags ","))
             (title-width (- (floor (* (frame-width) 0.7)) elfeed-search-trailing-width))
             (title-column (elfeed-format-column
                            title (elfeed-clamp
                                   elfeed-search-title-min-width
                                   title-width
                                   elfeed-search-title-max-width)
                            :left))
             (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
             (str (concat (propertize title-column 'face title-faces 'kbd-help title) " "
                          (propertize date 'face 'elfeed-search-date-face)
                          (when feed-title
                            (concat " " (propertize feed-title 'face 'elfeed-search-feed-face)))
                          (when tags (concat " " "(" tags-str ")"))
                          (when domain (concat "\t" domain (when path path)))
                          (concat "\t" (propertize "elfeed" 'face 'consult-web-source-face))
                          )))
        (if consult-web-highlight-matches
            (cond
             ((listp match-str)
              (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
             ((stringp match-str)
              (setq str (consult-web--highlight-match match-str str t)))))
        (push (propertize str
                          :source "elfeed"
                          :title title
                          :url url
                          :search-url nil
                          :query query
                          :entry entry
                          :tags tags
                          :date date
                          :id id
                          :feed feed
                          )
              annotated-entries)))))

(defun consult-web--elfeed-search-buffer ()
  "Get or create buffer for `consult-web-elfeed'"
  (get-buffer-create (or consult-web-elfeed-search-buffer-name "*consult-web-elfeed-search*")))

(cl-defun consult-web--elfeed-fetch-results (input &rest args &key callback &allow-other-keys)
  "Return entries matching INPUT in elfeed database.
uses INPUT as filter ro find entries in elfeed databse.
if FILTER is non-nil, it is used as additional filter parameters.
"
(cl-letf* (((symbol-function #'elfeed-search-buffer) #'consult-web--elfeed-search-buffer))
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (maxcount (plist-get opts :count))
               (filter (and (plist-member opts :filter) (plist-get opts :filter)))
               (maxcount (or (and (integerp maxcount) maxcount)
                             (and maxcount (string-to-number (format "%s" maxcount)))
                             consult-web-default-count))
               (elfeed-search-filter (concat (if maxcount (format "#%d " maxcount))
                                             (if filter (format "%s" filter)
                                               consult-web-elfeed-default-filter)
                                             (if query (format "%s" query))
                                             ))
               (filter (elfeed-search-parse-filter elfeed-search-filter))
               (head (list nil))
               (tail head)
               (count 0)
               (lexical-binding t)
               (search-func (byte-compile (elfeed-search-compile-filter filter))))
    (with-elfeed-db-visit (entry feed)
      (when (funcall search-func entry feed count)
        (setf (cdr tail) (list entry)
              tail (cdr tail)
              count (1+ count))))
    (when-let ((entries (cdr head)))
      (consult-web--elfeed-format-candidate entries query)))
      ))


(defun consult-web--elfeed-preview (cand)
  "Shows a preview buffer of CAND for `consult-web-elfeed'.
Uses `elfeed-show-entry'."
  (if (listp cand) (setq cand (or (car-safe cand) cand)))
  (let* ((entry (get-text-property 0 :entry cand))
         (buff (get-buffer-create (elfeed-show--buffer-name entry))))
    (with-current-buffer buff
      (elfeed-show-mode)
      (setq elfeed-show-entry entry)
      (elfeed-show-refresh))
    (funcall (consult--buffer-preview) 'preview
             buff
             )))


(consult-web-define-source "elfeed"
                           :narrow-char ?e
                           :type 'sync
                           :face 'elfeed-search-unread-title-face
                           :request #'consult-web--elfeed-fetch-results
                           :on-preview #'consult-web--elfeed-preview
                           :on-return #'identity
                           :on-callback #'consult-web--elfeed-preview
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (boundp 'elfeed-db))
                           :group #'consult-web--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-web-elfeed' module

(provide 'consult-web-elfeed)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-elfeed)
;;; consult-web-elfeed.el ends here
