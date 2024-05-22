;;; consult-web-mu4e.el --- Consulting Mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-mu "1.0") (consult-web "0.2"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-web)
(require 'consult-mu)

(defun consult-web-mu--format-candidate (cand highlight)
  "Formats candidates for `consult-web-mu4e'
"

  (let* ((string (car cand))
         (info (cadr cand))
         (msg (plist-get info :msg))
         (query (plist-get info :query))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (headers-template (consult-mu--headers-template))
         (str (if headers-template
                 (consult-mu--expand-headers-template msg headers-template)
                  string)
         )
         (str (propertize str :msg msg :query query :type :dynamic :source "mu4e" :title string))
         )
         (if (and consult-mu-highlight-matches highlight)
                     (cond
                      ((listp match-str)
                       (mapcar (lambda (match) (setq str (consult-mu--highlight-match match str t))) match-str))
                      ((stringp match-str)
                       (setq str (consult-mu--highlight-match match-str str t))))
           str)
(when msg
  (cons str (list :msg msg :query query :type :dynamic)))))

(cl-defun consult-web--mu-fetch-results (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “mu4e”.
"
  (save-mark-and-excursion
  (consult-mu--execute-all-marks)
  )
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-web-default-count))
               (mu-input (format "%s -- --maxnum %s" query count))
               (messages)
               )
    (consult-mu--update-headers mu-input nil nil :dynamic)
    (with-current-buffer consult-mu-headers-buffer-name
      (goto-char (point-min))
     (setq messages (remove nil
              (cl-loop until (eobp)
                       collect (let ((msg (ignore-errors (mu4e-message-at-point))))
                                 (consult-web-mu--format-candidate `(,(buffer-substring (point) (point-at-eol)) (:msg ,(ignore-errors (mu4e-message-at-point)) :query ,input)) t))
                 do (forward-line 1)))
           ))
    (when (and messages callback)
      (funcall callback messages))))

(defun consult-web--mu-preview (cand)
  "Preview for mu4e candidates"
  (when-let* ((info (text-properties-at 0 (cdr (get-text-property 0 'multi-category cand))))
              (msg (plist-get info :msg))
              (query (plist-get info :query))
              (msgid (substring-no-properties (plist-get msg :message-id)))
              (match-str (car (consult--command-split query)))
              (match-str (car (consult--command-split query)))
              (mu4e-headers-buffer-name consult-mu-headers-buffer-name)
              (buffer consult-mu-view-buffer-name))
    (add-to-list 'consult-mu--view-buffers-list buffer)
    (funcall (consult--buffer-preview) 'preview
             (consult-mu--view msg t consult-mu-mark-previewed-as-read match-str)
             )
    (with-current-buffer consult-mu-view-buffer-name
      (unless (one-window-p) (delete-other-windows))
      ))
  )

(defun consult-web--mu-return (cand)
  "return function for mu4e candidates"
(save-mark-and-excursion
  (consult-mu--execute-all-marks)
  )
(setq consult-mu--override-group nil)
cand
)

(defun consult-web--mu-callback (cand)
  "Callback function for mu4e candidates"
  (let* ((info (text-properties-at 0 (cdr (get-text-property 0 'multi-category cand))))
         (msg (plist-get info :msg))
         (query (plist-get info :query))
         (match-str (car (consult--command-split query)))
         )
    (consult-mu--view msg nil consult-mu-mark-viewed-as-read match-str)
    (consult-mu-overlays-toggle consult-mu-view-buffer-name)
    )
)

(consult-web-define-source "mu4e"
                           :narrow-char ?m
                           :type 'dynamic
                           :category 'consult-mu-messages
                           :face 'consult-web-engine-source-face
                           :request #'consult-web--mu-fetch-results
                           :lookup #'consult-mu--lookup
                           :on-preview #'consult-web--mu-preview
                           :on-return #'consult-web--mu-return
                           :on-callback #'consult-web--mu-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :group #'consult-web--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-web-mu4e' module

(provide 'consult-web-mu4e)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-gh)
;;; consult-web-mu4e.el ends here
