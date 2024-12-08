;;; consult-web-elfeed.el --- Consulting Elfeed -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: "0.1"
;; Package-Requires: ((emacs "28.1") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-web/blob/main/consult-web-sources
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'elfeed)
(require 'consult-web)

;;; Customization Variables
(defcustom consult-web-elfeed-search-buffer-name "*consult-web-elfeed-search*"
  "Name for consult-web-elfeed-search buffer."
  :type 'string)

(defcustom consult-web-elfeed-default-filter nil
  "Default Filter for consult-web-elfeed-search."
  :type 'string)

(defun consult-web-dynamic--elfeed-format-candidate (table &optional face &rest args)
  "Returns a formatted string for candidates of `consult-web-elfeed'.

TABLE is a hashtable from `consult-web--elfeed-fetch-results'."
  (let* ((pl (consult-web-hashtable-to-plist table))
         (title (format "%s" (gethash :title table)))
         (url (gethash :url table))
         (urlobj (if url (url-generic-parse-url url)))
         (domain (if (url-p urlobj) (url-domain urlobj)))
         (domain (if (stringp domain) (propertize domain 'face 'consult-web-domain-face)))
         (path (if (url-p urlobj) (url-filename urlobj)))
         (path (if (stringp path) (propertize path 'face 'consult-web-path-face)))
         (source (gethash :source table))
         (source (if (stringp source) (propertize source 'face 'consult-web-source-face)))
         (query (gethash :query table))
         (date (gethash :date table))
         (date (if (stringp date) (propertize date 'face 'consult-web-path-face)))
         (tags (gethash :tags table))
         (tags (cond
                ((listp tags)
                     (mapconcat (lambda (item) (format "%s" item)) tags " "))
                ((stringp tags)
                 tags)
                (t
                 (format "%s" tags))))
         (tags (propertize tags 'face 'consult-web-source-face))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or face 'consult-web-scholar-source-face)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :query :source :id :tags :date :filter)))
          (str (concat title-str
                       (if domain (concat "\t" domain (if path path)))
                       (if date (format "\s\s%s" date))
                       (if tags (format "\s\s%s" tags))
                       (if source (format "\t%s" source))
                       (if extra-args (format "\s\s%s" extra-args))))
         (str (apply #'propertize str pl))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defun consult-web--elfeed-search-buffer ()
  "Get or create buffer for `consult-web-elfeed'"
  (get-buffer-create (or consult-web-elfeed-search-buffer-name "*consult-web-elfeed-search*")))


(defun consult-web--elfeed-search (input entries)
  "Convert elfeed search tnries to hashtables for `consult-web-elfeed'.

Returns a list of hashtables, each presenting one elfeed feed."
  (cl-loop for entry in entries
           collect (let* ((table (make-hash-table :test 'equal))
                          (title (elfeed-entry-title entry))
                          (url (elfeed-entry-link entry))
                          (date (format-time-string "%Y-%m-%d %H:%M" (elfeed-entry-date entry)))
                          (id (elfeed-entry-id entry))
                          (tags (elfeed-entry-tags entry))
                          )
                     (puthash :title title
                              table)
                     (puthash :url url
                              table)
                     (puthash :date date
                              table)
                     (puthash :tags tags
                              table)
                     (puthash :id id
                              table)
                     (puthash :source "elfeed"
                              table)
                     (puthash :query input
                             table)
                     table)))

(cl-defun consult-web--elfeed-fetch-results (input &rest args &key filter &allow-other-keys)
  "Return entries matching INPUT in elfeed database.
uses INPUT as filter ro find entries in elfeed databse.
if FILTER is non-nil, it is used as additional filter parameters.
"
  (cl-letf* (((symbol-function #'elfeed-search-buffer) #'consult-web--elfeed-search-buffer))
    (let* ((input (if consult-web-elfeed-default-filter
                      (concat input " " consult-web-elfeed-default-filter)
                    input))
          (new-filter (if (member :filter args)
                          (concat input " " (format "%s" filter))
                        input)))
      (setq elfeed-search-filter new-filter)
      (elfeed-search-update :force)
      (with-current-buffer (consult-web--elfeed-search-buffer)
        (elfeed-search-mode)
        (save-mark-and-excursion
          (goto-char (point-min))
          (mark-whole-buffer)
          (consult-web--elfeed-search input (elfeed-search-selected))
          )))))

(defun consult-web--elfeed-preview (cand)
 "Shows a preview buffer of CAND for `consult-web-elfeed'.

Uses `elfeed-show-entry'."
  (let*  ((id (cond ((listp cand)
                             (get-text-property 0 :id (car cand)))
                            (t
                             (get-text-property 0 :id cand))))
          (entry (elfeed-db-get-entry id))
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
                           :face 'elfeed-search-unread-title-face
                           :request #'consult-web--elfeed-fetch-results
                           :format #'consult-web-dynamic--elfeed-format-candidate
                           :on-preview #'consult-web--elfeed-preview
                           :on-return #'identity
                           :on-callback #'consult-web--elfeed-preview
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :dynamic 'both
                           )

;;; provide `consult-web-elfeed' module

(provide 'consult-web-elfeed)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-elfeed)
;;; consult-web-elfeed.el ends here
