;;; consult-web-embark.el --- Emabrk Actions for `consult-web' -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish


;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (consult "0.34") (consult-web 0.2))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

;;; Requirements

(require 'embark)
(require 'consult-web)

;;; Define Embark Action Functions

(defun consult-web-embark-default-action (cand)
  "Calls the default action on CAND.

Gets the default callback function from `consult-web-sources-alist'."
  (let* ((source (and (stringp cand) (get-text-property 0 :source cand))))
    (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) cand))
  )

(add-to-list 'embark-default-action-overrides '(consult-web . consult-web-embark-default-action))


(defun consult-web-embark-insert-title (cand)
  "Insert the title oif the candidate at point"
  (if-let ((title (and (stringp cand) (get-text-property 0 :title cand))))
      (insert (format " %s " title))))

(defun consult-web-embark-copy-title-as-kill (cand)
  "Copy the title of the candidate to `kill-ring'."
  (if-let ((title (and (stringp cand) (get-text-property 0 :title cand))))
      (kill-new (string-trim title))))

(defun consult-web-embark-insert-url-link (cand)
  "Insert the title oif the candidate at point."
  (let* ((url (and (stringp cand) (get-text-property 0 :url cand)))
         (url (and (stringp url) (string-trim url)))
         (title (and (stringp cand) (get-text-property 0 :title cand))))
    (when url
      (cond
       ((derived-mode-p 'org-mode)
        (insert (cond
                 ((and url title) (format " [[%s][%s]] " url title))
                 (url (format " [[%s]] " url))
                 (t ""))
                ))
       ((derived-mode-p 'markdown-mode)
        (insert (cond
                 ((and url title) (format " [%s](%s) " url title))
                 (url (format " <%s> " url))
                 (t ""))
                ))
       (t
        (insert (cond
                 ((and url title) (format " %s (%s) " title  url))
                 (url (format " %s " url))
                 (t ""))
                ))))))

(defun consult-web-embark-copy-url-as-kill (cand)
  "Copy the url of the candidate to `kill-ring'."
  (if-let ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (kill-new (format " %s " (string-trim url)))
    ))

(defun consult-web-embark-external-browse-link (cand)
  "Open the url with `consult-web-default-browse-function'"
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (funcall consult-web-default-browse-function url)))

(defun consult-web-embark-alternate-browse-link (cand)
  "Open the url with `consult-web-alternate-browse-function'"
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (funcall consult-web-alternate-browse-function url)))

(defun consult-web-embark-external-browse-search-link (cand)
  "Open the search url (the search engine page) in the external browser."
  (if-let* ((search-url (and (stringp cand) (get-text-property 0 :search-url cand))))
      (funcall #'browse-url search-url)))

(defun consult-web-embark-show-preview (cand)
  "Get a preview of CAND.

Gets the preview function from `consult-web-sources-alist'."
  (let* ((source (and (stringp cand) (get-text-property 0 :source cand))))
    (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-preview) cand))
  )

;;; Define Embark Keymaps

(defvar-keymap consult-web-embark-general-actions-map
  :doc "Keymap for consult-web-embark"
  :parent embark-general-map
  "i t"  #'consult-web-embark-insert-title
  "i u" #'consult-web-embark-insert-url-link
  "w t" #'consult-web-embark-copy-title-as-kill
  "w u" #'consult-web-embark-copy-url-as-kill
  "o o" #'consult-web-embark-external-browse-link
  "o O" #'consult-web-embark-alternate-browse-link
  "o s" #'consult-web-embark-external-browse-search-link
  "o p" #'consult-web-embark-show-preview
  )


(add-to-list 'embark-keymap-alist '(consult-web . consult-web-embark-general-actions-map))

(defun consult-web-embark-scholar-external-browse-doi (cand)
  "Open the DOI url in external browser"
  (if-let* ((doi (and (stringp cand) (get-text-property 0 :doi cand))))
      (funcall #'browse-url (concat "https://doi.org/" doi))))

(defun consult-web-embark-scholar-copy-authors-as-kill (cand)
  "Copy the authors of the candidate to `kill-ring'."
  (if-let ((authors (and (stringp cand) (get-text-property 0 :authors cand))))
      (kill-new (string-trim (format " %s " authors)))
    ))

(defun consult-web-embark-scholar-insert-authors (cand)
  "Insrt the authors of the candidate at point."
  (if-let ((authors (and (stringp cand) (get-text-property 0 :authors cand))))
      (insert (string-trim (mapconcat #'identity authors ", ")))
    ))

(defvar-keymap consult-web-embark-scholar-actions-map
  :doc "Keymap for consult-web-embark-scholar"
  :parent consult-web-embark-general-actions-map
  "o d" #'consult-web-embark-scholar-external-browse-doi
  "w a" #'consult-web-embark-scholar-copy-authors-as-kill
  "i a" #'consult-web-embark-scholar-insert-authors
  )

(add-to-list 'embark-keymap-alist '(consult-web-scholar . consult-web-embark-scholar-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-web-scholar . consult-web-embark-default-action))



(defvar-keymap consult-web-embark-video-actions-map
  :doc "Keymap for consult-web-embark-video"
  :parent consult-web-embark-general-actions-map
  )

(add-to-list 'embark-keymap-alist '(consult-web-video . consult-web-embark-video-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-web-video . consult-web-embark-default-action))

;;; Provide `consul-web-embark' module

(provide 'consult-web-embark)

;;; consult-web-embark.el ends here
