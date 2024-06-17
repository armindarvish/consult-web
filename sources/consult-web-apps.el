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
(require 'xdg)

(defcustom consult-web-apps-paths (list)
  "List of paths to directories containing applications.
"
  :type '(repeat :tag "List of paths" directory))

(defcustom consult-web-apps-open-command-args nil
  "Command line args to open an application"
  :type 'string)

(defcustom consult-web-apps-regexp-pattern ""
"Regexp pattern to find system applications"
:type 'regexp)

(defcustom consult-web-apps-default-launch-function #'consult-web--apps-lauch-app
  "consult-web default function to launch an app"
  :type '(choice (function :tag "(Default) Use System Shell" consult-web--apps-lauch-app)
                 (function :tag "Custom Function")))

(pcase system-type
  ('darwin
   (setq consult-web-apps-paths (append (file-expand-wildcards "/Applications/Adobe*") (list "/Applications" "/Applications/Utilities/" "/System/Applications/" "/System/Applications/Utilities/")))
   (setq consult-web-apps-regexp-pattern ".*\\.app$")
   (setq consult-web-apps-open-command-args "open -a")
   )
   ('gnu/linux
    (setq consult-web-apps-xdg-data-home (if (fboundp 'xdg-data-home) (xdg-data-home)
                                           (let ((path (getenv "XDG_DATA_HOME")))
                                             (if (or (null path) (string= path ""))
                                                 nil
                                               (parse-colon-path path)))))
     (setq consult-web-apps-xdg-data-dirs (if (fboundp 'xdg-data-dirs) (xdg-data-dirs)
                                           (let ((path (getenv "XDG_DATA_DIRS")))
                                             (if (or (null path) (string= path ""))
    nil
                                               (parse-colon-path path)))))
     (setq consult-web-apps-paths (remove nil (mapcar (lambda (dir)
                                       (let ((path (and (stringp dir) (file-exists-p dir) (file-truename (expand-file-name "applications" dir)))))
                                              (and (stringp path) path)))
                                          (list consult-web-apps-xdg-data-home
                                               consult-web-apps-xdg-data-dirs
                                               "/usr/share"
                                               "/usr/local/share"))))
     (setq consult-web-apps-regexp-pattern ".*\\.desktop$")
     (setq consult-web-apps-open-command-args "gtk-launch")
    )
)

(defun consult-web--apps-cmd-args (app &optional file)
  (append (consult--build-args consult-web-apps-open-command-args)
          (list (format "%s" app))
          (if (and file (file-exists-p (file-truename file))) (list (format "%s" file)))))

(defun consult-web--apps-lauch-app (app &optional file)
  (let* ((name (concat "consult-web-" (file-name-base app)))
         (cmds (consult-web--apps-cmd-args app file)))
    (make-process :name name
                :connection-type 'pipe
                :command cmds
                )))

(defun consult-web--apps-preview (cand)
  "Mdfind preview function."
(funcall (consult--file-preview) 'preview cand))

(defun consult-web--apps-callback (cand)
  "Mdfind callback function."
  (let ((app (get-text-property 0 :app cand)))
    (funcall consult-web-apps-default-launch-function app)
  ))

(cl-defun consult-web--apps-format-candidates (&rest args &key source query title path face snippet visible &allow-other-keys)
"Formats the cnaiddates of `consult-web-apps.

Files are entries from `consult-web--apps-list-apps'.
QUERY is the query input from the user"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-web-source-face)))
         (directory (and path (file-name-directory path)))
         (directory (and (stringp directory) (propertize directory 'face 'consult-web-path-face)))
         ;; (filename (and path (file-name-nondirectory path)))
         ;; (filename (and (stringp filename) (propertize filename 'face 'consult-web-path-face)))
         (snippet (and (stringp snippet) (consult-web--set-string-width snippet (* 3 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-web-snippet-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-files-source-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 4 frame-width-percent)))
         (str (concat title-str
                      (unless visible "\s[Hidden App]")
                      (when snippet (concat "\t" snippet))
                      (when directory (concat "\t" directory))
                      ;; (when filename (concat "\t" (when directory directory) filename))
                      (when source (concat "\t" source))
                      )))
     (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defun consult-web--apps-get-desktop-apps ()
  "Return a list of files for system apps

Finds all files that match `consult-web-apps-regexp-pattern'
in `consult-web-apps-paths'.
"
  (let ((paths (if (stringp consult-web-apps-paths)
                   (list consult-web-apps-paths)
                 consult-web-apps-paths)))
    (when (listp paths)
      (cl-remove-duplicates (apply #'append (mapcar (lambda (path)
                          (when (file-exists-p path)
                          (directory-files path t consult-web-apps-regexp-pattern t))) paths))))))

(defun consult-web--apps-parse-app-file (file)
  (pcase system-type
         ('darwin
          (let ((name (file-name-base file))
                (comment nil)
                (exec (consult-web--apps-cmd-args (file-name-nondirectory file))))
            (list name comment exec t)))
         ('gnu/linux
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
                  (end (re-search-forward "^\\[" nil t))
                  (visible t)
                  name comment exec)
              (catch 'break
                (unless start
                  (throw 'break nil))

                (goto-char start)
                (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
                  (setq visible nil))
                (setq name (match-string 1))

                (goto-char start)
                (unless (re-search-forward "^Type *= *Application *$" end t)
                  (throw 'break nil))
                (setq name (match-string 1))

                (goto-char start)
                (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
                  (throw 'break nil))
                (setq name (match-string 1))

                (goto-char start)
                (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
                  (setq comment (match-string 1)))

                (goto-char start)
                (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
                  ;; Don't warn because this can technically be a valid desktop file.
                  (throw 'break nil))
                (setq exec (match-string 1))

                (goto-char start)
                (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
                  (let ((try-exec (match-string 1)))
                    (unless (locate-file try-exec exec-path nil #'file-executable-p)
                      (throw 'break nil))))

                  (list name comment exec visible)))))))

(cl-defun consult-web--apps-list-apps (input &rest args &key callback &allow-other-keys)
  "get a list of applications from OS.
"
 (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (count (or (and (integerp count) count)
                             (and count (string-to-number (format "%s" count)))
                             consult-web-default-count))
               (files (consult-web--apps-get-desktop-apps)))
   (mapcar (lambda (file)
             (pcase-let* ((source "Apps")
                          (`(,name ,comment ,exec ,visible) (consult-web--apps-parse-app-file file))
                    (title (or name (file-name-base file) ""))
                    (app (and (stringp file) (file-exists-p file ) (file-name-nondirectory file)))
                    (search-url nil)
                    (decorated (funcall #'consult-web--apps-format-candidates :source source :query query :title title :path file :snippet comment :visible visible)))
               (propertize decorated
                           :source source
                           :title title
                           :url nil
                           :search-url nil
                           :query query
                           :snippet comment
                           :path file
                           :exec exec
                           :app app)))
           (if query
               (seq-filter (lambda (file) (string-match (concat ".*" query ".*") file nil t)) files)
             files)
   )
 ))

(consult-web-define-source "Apps"
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
