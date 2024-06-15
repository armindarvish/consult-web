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

(defcustom consult-web-apps-open-command nil
  "Command line args to open an application"
  :type 'string)

(defcustom consult-web-apps-default-launch-function #'consult-web--apps-lauch-app
  "consult-web default function to launch an app"
  :type '(choice (function :tag "(Default) Use System Shell" consult-web--apps-lauch-app)
                 (function :tag "Custom Function")))

(defalias 'counsel--xdg-data-home
  (if (fboundp 'xdg-data-home)
      #'xdg-data-home
    (lambda ()
      (let ((directory (getenv "XDG_DATA_HOME")))
        (if (or (null directory) (string= directory ""))
            "~/.local/share"
          directory))))
  "Compatibility shim for `xdg-data-home'.")

(defalias 'counsel--xdg-data-dirs
  (if (fboundp 'xdg-data-dirs)
      #'xdg-data-dirs
    (lambda ()
      (let ((path (getenv "XDG_DATA_DIRS")))
        (if (or (null path) (string= path ""))
            '("/usr/local/share" "/usr/share")
          (parse-colon-path path)))))
  "Compatibility shim for `xdg-data-dirs'.")


(pcase system-type
  ('darwin
   (setq consult-web-apps-paths (list "/Applications" "/Applications/Utilities/" "/System/Applications/" "/System/Applications/Utilities/"))
   (setq consult-web-apps-open-command "open -a"))
   ('gnu/linux
    (setq consult-web-apps-xdg-data-home (if (fboundp 'xdg-data-home) (xdg-data-home)
                                           (let ((path (getenv "XDG_DATA_HOME")))
                                             (if (or (null path) (string= path ""))
                                                 '("/usr/local/share" "/usr/share")
                                               (parse-colon-path path)))))
     (setq consult-web-apps-xdg-data-dirs (if (fboundp 'xdg-data-dirs) (xdg-data-dirs)
                                           (let ((path (getenv "XDG_DATA_DIRS")))
                                             (if (or (null path) (string= path ""))
                                                 '("/usr/local/share" "/usr/share")
                                               (parse-colon-path path)))))
     (setq consult-web-apps-paths (mapcar (lambda (dir) (expand-file-name "applications" dir))
	                                 (list consult-web-apps-xdg-data-home
                                               consult-web-apps-xdg-data-dirs)))
     (setq consult-web-apps-open-command "")
    )
)

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
         (directory (and (stringp directory) (propertize directory 'face 'consult-web-path-face)))
         ;; (filename (and path (file-name-nondirectory path)))
         ;; (filename (and (stringp filename) (propertize filename 'face 'consult-web-path-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-files-source-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 4 frame-width-percent)))
         (str (concat title-str
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

(defun consult-web--apps-list-osx-apps ()
  (let ((paths (if (stringp consult-web-apps-paths)
                   (list consult-web-apps-paths)
                 consult-web-apps-paths)))
    (when (listp paths)
      (apply #'append (mapcar (lambda (path)
                          (file-expand-wildcards (concat path "/" consult-web-apps-pattern))) paths)))))

(defvar consult-web--apps-linux-faulty-entries (list))

(defun consult-web-apps-linux-format-candidate (name comment _exec)
  "Format Linux application names with the NAME (and COMMENT) only, but pretty."
  (format "% -45s%s"
          (propertize
           (consult-web--set-string-width name 45)
           'face 'consult-web-files-source-face)
          (if comment
              (concat ": " comment)
            "")))

(defun consult-web--apps-linux-get-desktop-entries ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order.

Taken from counsel.
See URL `https://github.com/abo-abo/swiper'"
  (let ((paths (if (stringp consult-web-apps-paths)
                   (list consult-web-apps-paths)
                 consult-web-apps-paths))
        (hash (make-hash-table :test #'equal))
	result)
    (dolist (dir paths)
      (when (file-exists-p dir)
	(let ((dir (file-name-as-directory dir)))
	  (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
	    (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
	      (when (and (not (gethash id hash)) (file-readable-p file))
		(push (cons id file) result)
		(puthash id file hash)))))))
    result))

(defun consult-web--apps-linux-parse-desktop-entry (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
          (end (re-search-forward "^\\[" nil t))
          (visible t)
          name comment exec)
      (catch 'break
        (unless start
          (push file consult-web--apps-linux-faulty-entries)
          (message "Warning: File %s has no [Desktop Entry] group" file)
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
          (push file consult-web--apps-linux-faulty-entries)
          (message "Warning: File %s has no Name" file)
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
        (propertize
         (funcall #'consult-web-apps-linux-format-candidate name comment exec)
         'visible visible)))))

(defun cosult-web--apps-linux-parse-desktop-entries (entries)
  "Parse the given alist of Linux desktop entries.
Each entry in DESKTOP-ENTRIES-ALIST is a pair of ((id . file-name)).
Any desktop entries that fail to parse are recorded in
`counsel-linux-apps-faulty'."
  (let (result)
    (setq consult-web--apps-linux-faulty-entries nil)
    (dolist (entry entries result)
      (let* ((id (car entry))
             (file (cdr entry))
             (r (consult-web--apps-linux-parse-desktop-entry file)))
        (when r
          (push (cons r id) result))))))

(defun consult-web--apps-linux-list-apps ()
  (cosult-web--apps-linux-parse-desktop-entries (consult-web--apps-linux-get-desktop-entries)))

(cl-defun consult-web--apps-list-apps (input &rest args &key callback &allow-other-keys)
  "get a list of applications from OS.
"
 (pcase-let* ((`(,query . ,opts) (consult-web--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (count (or (and (integerp count) count)
                             (and count (string-to-number (format "%s" count)))
                             consult-web-default-count))
               (files (pcase system-type
                        ('darwin (consult-web--apps-list-osx-apps))
                        ('gnu/linux (consult-web--apps-list-linux-apps)))))
   (mapcar (lambda (file)
             (let* ((source "Applications")
                    (title (or (file-name-base file) ""))
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
