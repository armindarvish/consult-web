;;; consult-web-apps.el --- Consulting OS applications -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-web "0.2"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-web)

(defcustom consult-web-apps-paths (pcase system-type
                                           ('darwin (list "/Applications" "/Applications/Utilities/" "/System/Applications/" "/System/Applications/Utilities/"))
                                           ('gnu/linux
                                            "/applications")
                                           )
  "List of paths to directories containing applications.
"
  :type '(repeat :tag "List of paths" string))

(defcustom consult-web-apps-open-command (pcase system-type
                                           ('darwin "open -a")
                                           ('gnu/linux ""))
  "Command line args to open an application"
  :type 'string)


(defcustom consult-web-apps-default-launch-function #'consult-web--apps-lauch-app
  "consult-web default function to launch an app"
  :type '(choice (function :tag "(Default) Use System Shell" consult-web--apps-lauch-app)
                 (function :tag "Custom Function")))

(defvar consult-web-apps-pattern "*.app"
"Regexp pattern to find OSX applications")

(defun consult-web--apps-cmd-args (app &optional file)
  (concat consult-web-apps-open-command
          " "
          (shell-quote-argument app)
          (if (and file (file-exists-p (file-truename file))) (shell-quote-argument file))))

(defun consult-web--apps-lauch-app (app &optional file)
  (start-process-shell-command "consult-web-apps" nil (consult-web--apps-cmd-args app file)
                               ))

(defun consult-web--apps-preview (cand)
  "Mdfind preview function."
(funcall (consult--file-preview) 'preview cand))

(defun consult-web--apps-callback (cand)
  "Mdfind callback function."
  (let* ((app (get-text-property 0 :title cand)))
    (funcall consult-web-apps-default-launch-function app)
  ))

(cl-defun consult-web--apps-format-candidates (&rest args &key source query title path face &allow-other-keys)
"Formats the cnaiddates of `consult-web-apps.

Files are entries from `consult-web--apps-list-apps'.
QUERY is the query input from the user"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-web-source-face)))
         (directory (and path (file-name-directory path)))
         (directory (and (stringp directory) (propertize directory 'face 'consult-web-domain-face)))
         (filename (and path (file-name-nondirectory path)))
         (filename (and (stringp filename) (propertize filename 'face 'consult-web-path-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-files-source-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 4 frame-width-percent)))
         (str (concat title-str
                      (when filename (concat "\t" (when directory directory) filename))
                      (when source (concat "\t" source))
                      )))
     (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(cl-defun consult-web--apps-list-apps (input &rest args &key callback &allow-other-keys)
  "get a list of applications from OS.
"
 (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (count (or (and (integerp count) count)
                             (and count (string-to-number (format "%s" count)))
                             consult-web-default-count))
               (paths (if (stringp consult-web-apps-paths)
                   (list consult-web-apps-paths)
                 consult-web-apps-paths))
               (files (apply #'append (mapcar (lambda (path)
                                                (file-expand-wildcards (concat path "/" consult-web-apps-pattern))) paths))))
   (mapcar (lambda (file)
             (let* ((source "Applications")
                    (title (file-name-base file))
                    (search-url nil)
                    (decorated (funcall #'consult-web--apps-format-candidates :source source :query query :title title :path file)))
               (propertize decorated
                           :source source
                           :title title
                           :url nil
                           :search-url nil
                           :query query
                           :snippet nil
                           :path file)))
           (seq-filter (lambda (file) (string-match (concat ".*" query ".*") file nil t)) files)
   )
 ))

(consult-web-define-source "Applications"
                           :narrow-char ?a
                           :type 'sync
                           :request #'consult-web--apps-list-apps
                           :on-preview #'ignore
                           :on-return #'identity
                           :on-callback #'consult-web--apps-callback
                           :preview-key consult-web-preview-key
                           :search-history 'consult-web--search-history
                           :selection-history 'consult-web--selection-history
                           :enabled (lambda () (boundp 'consult-web-apps-paths))
                           :group #'consult-web--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           :category 'file
                           )

;;; provide `consult-web-apps module

(provide 'consult-web-apps)

(add-to-list 'consult-web-sources-modules-to-load 'consult-web-apss)
;;; consult-web-apps.el ends here
