;;; consult-web.el --- Consulting Web Search Engines -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-web
;; Keywords: convenience

;;; Commentary:

;;; Code:

;;; Requirements
(eval-when-compile
  (when (featurep 'json)
    (require 'json))
  (when (featurep 'request)
    (require 'request))
  (when (featurep 'plz)
    (require 'plz))
  )
(require 'consult)
(require 'url)
(require 'url-queue)

;;; Group
(defgroup consult-web nil
  "Consulting search engines and AI assistants"
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :group 'web
  :group 'search
  :prefix "consult-web-")

;;; Customization Variables
(defcustom consult-web-sources-modules-to-load  (list)
  "List of source modules/features to load.

This variable is a list of symbols;
each symbol being a source featue (e.g. consult-web-brave)"
  :type '(repeat :tag "list of source modules/features to load" symbol))


(defcustom consult-web-default-browse-function #'browse-url
  "consult-web default function when selecting a link"
  :type '(choice (function :tag "(Default) Browse URL" #'browse-url)
                 (function :tag "Custom Function")))


(defcustom consult-web-default-format-candidate #'consult-web--highlight-format-candidate
  "consult-web default function when selecting a link"
  :type '(choice (function :tag "(Default) Adds Metadata and Highlights Query" #'consult-web--highlight-format-candidate)
                 (function :tag "Simple and Fast Foramting (No Metadata)" #'consult-web--simple-format-candidate)
                 (function :tag "Custom Function")))


(defcustom consult-web-alternate-browse-function #'eww-browse-url
  "consult-web default function when selecting a link"
  :type '(choice (function :tag "(Default) EWW" #'eww-browse-url)
                 (function :tag "Custom Function")))

(defcustom consult-web-default-preview-function #'eww-browse-url
  "consult-web default function when previewing a link"
  :type '(choice (function :tag "(Default) EWW" #'eww-browse-url)
                 (function :tag "Custom Function")))


(defcustom consult-web-show-preview nil
  "Should`consult-web' show previews?
This turns previews on/off globally for all consult-web sources."
  :type 'boolean)

(defcustom consult-web-preview-key consult-preview-key
  "Preview key for consult-web.
This is similar to `consult-preview-key' but explicitly For consult-web."
  :type '(choice (const :tag "Any Key" Any)
                 (List :tag "Debounced"
                       (const :Debounce)
                       (Float :tag "Seconds" 0.1)
                       (const Any))
                 (const :tag "No Preview" nil)
                 (Key :tag "Key")
                 (repeat :tag "List Of Keys" Key)))

(defcustom consult-web-default-count 5
  "Number Of search results to retrieve."
  :type 'integer)

(defcustom consult-web-default-page 0
  "Offset of search results to retrieve.
If this is set to N, the first N “pages”
(or other first N entities, items for example,
depending On the source search engine capabilities)
of the search results are omitted and the rest are shown."
  :type 'integer)

(defcustom consult-web-default-timeout 30
  "Default timeout in seconds for synchronous requests."
  :type 'integer)

(defcustom consult-web-url-queue-parallel-processes 15
  "The number of concurrent url-retrieve processes."
  :type 'integer)

(defcustom consult-web-url-queue-timeout 120
  "How long to let a job live once it's started (in seconds)."
  :type '(integer :tag "Timeout in seconds"))

(defcustom consult-web-log-buffer-name " *consult-web-log*"
"String for consult-web-log buffer name"
:type 'string)

(defcustom consult-web-log-level nil
  "How to make logs for consult-web requests?
This can be set to nil, info or debug

nil: Does not log anything
info: Logs URLs and response's http header
debug: Logs URLs and the entire http response.

When non-nil, information is logged to `consult-web-log-buffer-name'."
  :type '(choice
          (const :tag "No Logging" nil)
          (const :tag "Just HTTP Header" info)
          (const :tag "Full Response" debug)))

(defcustom consult-web-group-by :source
  "What field to use to group the results in the minibuffer?

By default it is set to :source. but can be any of:

  nil       Do not group
  :title    group by candidate's string
  :url      group by URL
  :domain   group by the domain of the URL
  :source   group by source name
 "
  :type '(radio (const :tag "URL path" :url)
                (const :tag "Domain of URL path":domain)
                (const :tag "Name of the search engine or source" :source)
                (const :tag "Custom other field (constant)" :any)
                (const :tag "Do not group" nil)))


(defcustom consult-web-dynamic-sources  (list)
  "List of sources used by `consult-web-multi'.

This variable is a list of strings or symbols;
 - strings can be name of a source, a key from `consult-web-sources-alist',
which can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-source-from-consult-source'.
 - symbols can be other consult sources
(see `consult-buffer-sources' for example.)"
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-omni-sources  (list)
"List of sources used by `consult-web-omni'.

This variable is a list of strings or symbols;
 - strings can be name of a source, a key from `consult-web-sources-alist',
which can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-source-from-consult-source'.
 - symbols can be other consult sources
(see `consult-buffer-sources' for example.)"
:type '(repeat :tag "list of source names" (choice (string symbol))))


(defcustom consult-web-scholar-sources  (list)
  "List of sources used by `consult-web-scholar'.

This variable is a list of strings or symbols;
 - strings can be name of a source, a key from `consult-web-sources-alist',
which can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-source-from-consult-source'.
 - symbols can be other consult sources
(see `consult-buffer-sources' for example.)"
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-static-sources  (list)
  "List of sources used by `consult-web-static'.

This variable is a list of strings or symbols;
 - strings can be name of a source, a key from `consult-web-sources-alist',
which can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-source-from-consult-source'.
 - symbols can be other consult sources
(see `consult-buffer-sources' for example.)"
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-highlight-matches t
  "Should `consult-web' highlight search queries in the minibuffer?"
  :type 'boolean)


(defcustom consult-web-default-interactive-command #'consult-web-omni
  "Which command should `consult-web' call?"
  :type '(choice (function :tag "(Default) Omni search with both local and web sources" #'consult-web-omni)
                 (function :tag "Multi web source (i.e. `consult-web-multi')"  #'consult-web-multi)
                 (function :tag "Scholar (academic literature) search (i.e. `consult-web-scholar')"  #'consult-web-scholar)
                 (function :tag "Other custom interactive command")))


(defcustom consult-web-http-retrieve-backend 'url
  "Which backend should `consult-web' use for http requests?"
  :type   '(choice
          (const :tag "(Default) Built-in Emacs's url-retrive" 'url)
          (const :tag "`emacs-request' backend" 'request)
          (const :tag "`plz' backend" 'plz)))

(defcustom consult-web-default-autosuggest-command nil
  "Which command should `consult-web' use for auto suggestion on search input?"
  :type '(choice (cons :tag "(Default) no autosuggestion" nil)
          (function :tag "Brave autosuggestion (i.e. `consult-web-brave-autosuggest')" #'consult-web-brave-autosuggest)
                 (function :tag "Google autosuggestion (i.e. `consult-web-dynamic-google-autosuggest')" #'consult-web-dynamic-google-autosuggest)
                 (function :tag "Other custom interactive command")))

(defcustom consult-web-dynamic-input-debounce consult-async-input-debounce
  "Input debounce for dynamic commands.

The dynamic collection process is started only when
there has not been new input for consult-web-dynamic-input-debounce seconds. This is similarto `consult-async-input-debounce' but
specifically for consult-web dynamic commands.

By default inherits from `consult-async-input-debounce'."
  :type '(float :tag "delay in seconds"))


(defcustom consult-web-dynamic-input-throttle consult-async-input-throttle
  "Input throttle for dynamic commands.

The dynamic collection process is started only every
`consult-web-dynamic-input-throttle' seconds. this is similar
to `consult-async-input-throttle' but specifically for
consult-web dynamic commands.

By default inherits from `consult-async-input-throttle'."
  :type '(float :tag "delay in seconds"))

(defcustom consult-web-dynamic-refresh-delay consult-async-refresh-delay
  "refreshing delay of the completion ui for dynamic commands.

The completion UI is only updated every
`consult-web-dynamic-refresh-delay' seconds.
This is similar to `consult-async-refresh-delay' but specifically
for consult-web dynamic commands.

By default inherits from `consult-async-refresh-delay'. "
  :type '(float :tag "delay in seconds"))

;;; Other Variables

(defvar consult-web-sources--all-modules-list (list)
"List of all source modules.")

(defvar consult-web-category 'consult-web
  "Category symbol for the consult-web seach")

(defvar consult-web-scholar-category 'consult-web-scholar
  "Category symbol for scholar search")

(defvar consult-web-video-category 'consult-web-video
  "Category symbol for video search")

(defvar consult-web--selection-history (list)
  "History variable that keeps selected items.")

(defvar consult-web--search-history (list)
  "History variable that keeps search terms.")

(defvar consult-web-sources-alist (list)
  "Alist of search engine or ai assistant sources.

This is an alist mapping source names to source property lists.
This alist is used to define how to process data form
a source (e.g. format data) or find what commands to run on
selecting candidates from a source, etc.

You can use the convinient macro `consult-web-define-source'
or the command `consult-web--make-source-from-consult-source'
to add to this alist.")

(defvar consult-web--hidden-buffers-list (list)
  "List of currently open hidden buffers")

(defvar consult-web--override-group-by nil
"Override grouping in `consult-group' based on user input.

This is used in dynamic collection to change grouping.")

(defconst consult-web-http-end-of-headers-regexp
  (rx (or "\r\n\r\n" "\n\n"))
  "Regular expression matching the end of HTTP headers.")

(defvar consult-web-async-processes (list)
  "List of processes for async candidates colleciton")


(defvar consult-web-dynamic-timers (list)
  "List of timers for dynamic candidates colleciton")

(defvar consult-web--async-log-buffer " *consult-web--async-log*"
 "name of buffer for logging async processes info")

;;; Faces

(defface consult-web-default-face
  `((t :inherit 'default))
"Default face used for listing items in minibuffer.")

(defface consult-web-prompt-face
  `((t :inherit 'font-lock-variable-use-face))
"The face used for prompts in minibuffer.")

(defface consult-web-engine-source-face
  `((t :inherit 'font-lock-variable-use-face))
"The face for search engine source types in minibuffer.")

(defface consult-web-ai-source-face
  `((t :inherit 'font-lock-operator-face))
"The face for AI assistant source types in minibuffer.")

(defface consult-web-files-source-face
  `((t :inherit 'font-lock-number-face))
"The face for file source types in minibuffer.")

(defface consult-web-notes-source-face
  `((t :inherit 'font-lock-warning-face))
"The face for notes source types in minibuffer.")

(defface consult-web-scholar-source-face
  `((t :inherit 'font-lock-function-call-face))
"The face for academic literature source types in minibuffer.")

(defface consult-web-source-face
  `((t :inherit 'font-lock-comment-face))
"The face for source annotation in minibuffer.")

(defface consult-web-date-face
  `((t :inherit 'font-lock-preprocessor-face))
"The face for date annotation in minibuffer.")

(defface consult-web-domain-face
  `((t :inherit 'font-lock-string-face))
"The face for domain annotation in minibuffer.")

(defface consult-web-path-face
  `((t :inherit 'font-lock-warning-face))
"The face for path annotation in minibuffer.")

(defface consult-web-snippet-face
  `((t :inherit 'font-lock-doc-face))
"The face for source annotation in minibuffer.")

(defface consult-web-keyword-face
  `((t :inherit 'font-lock-keyword-face))
"The face for source annotation in minibuffer.")

(defface consult-web-comment-face
  `((t :inherit 'font-lock-comment-face))
"The face for source annotation in minibuffer.")

(defface consult-web-highlight-match-face
  `((t :inherit 'consult-highlight-match))
  "Highlight match face for `consult-web'.")

(defface consult-web-preview-match-face
  `((t :inherit 'consult-preview-match))
  "Preview match face in `consult-web' preview buffers.")

(defun consult-web-properties-to-plist (string &optional ignore-keys)
"Returns a plist of the text properties of STRING.

Ommits keys in IGNORE-KEYs."
(let ((properties (text-properties-at 0 string))
      (pl nil))
  (cl-loop for k in properties
           when (keywordp k)
           collect (unless (member k ignore-keys) (push (list k (plist-get properties k)) pl)))
  (apply #'append pl)))

(defun consult-web-propertize-by-plist (item props)
"Propertizes ITEM by PROPS plist"
  (apply #'propertize item props))

;;; Bakcend Functions

(defun consult-web--set-string-width (string width &optional truncate-pos add-pos)
  "Sets the STRING width to a fixed value, WIDTH.

If the STRING is longer than WIDTH, it truncates the STRING
 and adds ellipsis, \"...\". if the STRING is shorter,
it adds whitespace to the STRING.
If TRUNCATE-POS is non-nil, it truncates from position pos in the STRING
If ADD-POS is non-nil, it adds whitespace to the end of STRING.
"
  (let* ((string (format "%s" string))
         (w (length string)))
    (when (< w width)
      (if (and add-pos (< add-pos w))
          (setq string (format "%s%s%s" (substring string 0 add-pos) (consult-web-propertize-by-plist (make-string (- width w) ?\s) (text-properties-at add-pos string)) (substring string add-pos)))
        (setq string (format "%s%s" (substring string) (make-string (- width w) ?\s)))))
    (when (> w width)
      (if (and truncate-pos (< truncate-pos (- width 3)) (>= truncate-pos 0))
          (setq string (format "%s%s%s" (substring string 0 truncate-pos) (propertize (substring string truncate-pos (+ truncate-pos 3)) 'display "...") (substring string (- 0 (- width truncate-pos 3)))))
        (setq string (format "%s%s"
                             (substring string 0 (- width 3))
                             (propertize  (substring string (- width 3) width) 'display "...")
                             (propertize (substring string width) 'invisible t)))))
    string))

(defun consult-web--justify-left (string prefix maxwidth)
  "Sets the width of STRING+PREFIX justified from left.
It uses `consult-web--set-string-width' and sets the width
 of the concatenate of STRING+PREFIX
(e.g. `(concat PREFIX STRING)`) within MAXWIDTH.
This can be used for aligning marginalia info in minibuffer."
  (let ((s (length string))
        (w (length prefix)))
    (if (> maxwidth w)
    (consult-web--set-string-width string (- maxwidth w) 0)
    string
          )
    ))

(defun consult-web--set-url-width (domain path width)
"It shortes (or adds whitespace) to DOMAIN+PATH
to fit within WIDTH
"
  (when (stringp domain)
    (let* ((path-width (and (stringp path) (length path)))
           (path-target-width (- width (length domain))))
        (cond
         ((<= path-target-width 0)
          (consult-web--set-string-width domain width))
         ((integerp path-width)
          (concat domain (consult-web--set-string-width path path-target-width (floor (/ path-target-width 2)))))
         (t
          (consult-web--set-string-width (concat domain path) width))))))

(defun consult-web--highlight-match (regexp str ignore-case)
  "Highlights REGEXP in STR.

If a regular expression contains capturing groups,
 only these are highlighted.
If no capturing groups are used, highlight the whole match.
Case is ignored, if ignore-case is non-nil.
(This is adapted from `consult--highlight-regexps'.)"
  (let ((i 0))
    (while (and (let ((case-fold-search ignore-case))
                  (string-match regexp str i))
                (> (match-end 0) i))
      (let ((m (match-data)))
        (setq i (cadr m)
              m (or (cddr m) m))
        (while m
          (when (car m)
            (add-face-text-property (car m) (cadr m)
                                     'consult-web-highlight-match-face nil str)
            )
          (setq m (cddr m))))))
  str)

(defun consult-web--overlay-match (match-str buffer ignore-case)
  "Highlights MATCH-STR in BUFFER using an overlay.
If IGNORE-CASE is non-nil, it uses case-insensitive match.

This is provided for convinience,
if needed in formating candidates or preview buffers."
(with-current-buffer (or (get-buffer buffer) (current-buffer))
  (remove-overlays (point-min) (point-max) 'consult-web-overlay t)
  (goto-char (point-min))
  (let ((case-fold-search ignore-case)
        (consult-web-overlays (list)))
    (while (search-forward match-str nil t)
      (when-let* ((m (match-data))
                  (beg (car m))
                  (end (cadr m))
                  (overlay (make-overlay beg end))
                  )
        (overlay-put overlay 'consult-web-overlay t)
        (overlay-put overlay 'face 'consult-web-highlight-match-face)
        )))))

(defun consult-web-overlays-toggle (&optional buffer)
  "Toggles overlay highlights in consult-web view/preview buffers."
(interactive)
(let ((buffer (or buffer (current-buffer))))
(with-current-buffer buffer
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'consult-web-overlay)
      (if (and (overlay-get o 'face) (eq (overlay-get o 'face) 'consult-web-highlight-match-face))
          (overlay-put o 'face nil)
         (overlay-put o 'face 'consult-web-highlight-match-face))
      )
))))

(defun consult-web--numbers-human-readable (number &optional unit separator base prefixes)
  "Convert number to a human-redable string.

SEPARATOR is a string placed between unmber and unit
UNIT is a string used as unit
BASE is the number base used to derive prefix
PREFIXES is a list of chars for each magnitue
(e.g. '(“” “K” “M” “G” ...) for none, kilo, mega, giga, ...
"
  (let* ((power (if (and base (numberp base)) (float base) 1000.0))
	(prefixes (or prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y" "R" "Q")))
        (number (pcase number
                 ((pred numberp)
                  number)
                 ((pred stringp)
                  (string-to-number number))
                 (_ 0))))

    (while (and (>= number power) (cdr prefixes))
      (setq number (/ number power)
	    prefixes (cdr prefixes)))
    (let* ((prefix (car-safe prefixes)))
      (format (if (and (< number 10)
                       (>= (mod number 1.0) 0.05)
                       (< (mod number 1.0) 0.95))
                  "%.1f%s%s%s"
	        "%.0f%s%s%s")
	      number
              prefix
              (or separator " ")
              unit))
    ))

(defun consult-web--make-url-string (url params &optional ignore-keys)
"Adds key value pairs in PARAMS to URL as “&key=val”.

PARMAS should be an alist with keys and values to add to the URL.
Does not add keys for the key in IGNORE-KEYS list."

  (let* ((url (if (equal (substring-no-properties url -1 nil) "?")
                 url
               (concat url "?")))
         (list (append (list url) (cl-loop for (key . value) in params
                                           collect
                                           (unless (member key ignore-keys)
                                             (format "&%s=%s" key value))))))
  (mapconcat #'identity list)))

(defun consult-web-hashtable-to-plist (hashtable &optional ignore-keys)
"Converts a HASHTABLE to a plist.

Ommits keys in IGNORE-KEYS."

(let ((pl nil))
    (maphash
     (lambda (k v)
       (unless (member k ignore-keys)
         (push (list k v) pl)))
     hashtable)
    (apply #'append pl)))

(defun consult-web-expand-variable-function (var)
"Call the function if VAR is a function"
  (if (functionp var)
                 (funcall var)
    var))

(defun consult-web--url-log (string)
  "Logs the response from `consult-web-url-retrieve-sync' in `consult-web-log-buffer-name'."
   (with-current-buffer (get-buffer-create consult-web-log-buffer-name)
     (goto-char (point-min))
     (insert "**********************************************\n")
     (goto-char (point-min))
     (insert (format-time-string "%F - %T%n" (current-time)))
     (insert string)
     (insert "\n")
     (goto-char (point-min))
     (insert "\n\n**********************************************\n")))

(defun consult-web--parse-http-response (&optional buffer)
  "Parse the first header line such as \"HTTP/1.1 200 OK\"."
(with-current-buffer (or buffer (current-buffer))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\=[ \t\n]*HTTP/\\(?1:[0-9\\.]+\\) +\\(?2:[0-9]+\\)" url-http-end-of-headers t)
    `(:http-version ,(match-string 1) :code ,(string-to-number (match-string 2)))))))

(defun consult-web--url-response-body (response-data)
"Extracts the response body from `url-retrieve'."
(plist-get response-data :data))

(defun consult-web--url-retrieve-error-handler (&rest args)
  "Handles errors for consult-web-url-retrieve functions."
  (message "consult-web: url-retrieve got an error: %s" (consult-web--parse-http-response)))

(cl-defun consult-web-url-queue-retrieve (url &rest settings &key (sync 'nil) (type "GET") params headers data parser callback error timeout &allow-other-keys)
  "Retrieves URL with settings.

Passes all the arguments to
`url-retrieve-queue' or `url-retrieve-snchronously'.

if SYNC is non-nil, it retrieves URL sunchronously
(see `url-retrieve-synchronously'.)

TYPE is the http request type (e.g. “GET”, “POST”)

PARAMS are parameters added to the base url
using `consult-web--make-url-string'.

HEADERS are headers passed to headers (e.g. `url-request-extra-headers').

DATA are http request data passed to data (e.g. `url-request-data').

PARSER is a function that is executed in the url-retrieve
response and the results are passed to CALLBACK. It is called wthout any arguments
in the response buffer (i.e. it called like (funcall PARSER))
This is for example suitable for #'json-read.

CALLBACK is the function that is executed when the request is complete.
It takes one argument, PARSED-DATA which is the output of the PARSER above.
(i.e. it is called like (funcall CALLBACK (funcall PARSER)))

ERROR is a function that handles errors. It is called without any arguments
in the response buffer.

TIMEOUT is the time in seconds for timing out synchronous requests.
This is ignored in async requests.

Note that this function uses `url-queue-retrieve', and
sets url-queue-parallel-processes and url-queue-timeout to `consult-web-url-queue-parallel-processes',
and `consult-web-url-queue-timeout', respectively.
"
  (let* ((url-request-method type)
         (url-request-extra-headers headers)
         (url-request-data data)
         (url-with-params (consult-web--make-url-string url params))
         (url-debug (if consult-web-log-level t nil))
         (url-queue-parallel-processes consult-web-url-queue-parallel-processes)
         (url-queue-timeout consult-web-url-queue-timeout)
         (response-data '(:status nil :data nil))
         (buffer (if sync
                     (if timeout
                         (with-timeout
                             (timeout
                              (setf response-data (plist-put response-data :status 'timeout))
                              nil)
                           (url-retrieve-synchronously url-with-params 'silent nil timeout))
                       (url-retrieve-synchronously url-with-params 'silent nil timeout))
                   (url-queue-retrieve url-with-params
                                 (lambda (status &rest args)
                                   (let* ((parsed-data (condition-case nil
                                                     (if parser (funcall parser) (buffer-substring (point-min) (point-max)))
                                                   (error (funcall error)))))
                                     (setf response-data (plist-put response-data :status status))
                                     (when parsed-data
                                       (setf response-data (plist-put response-data :data (funcall callback parsed-data)))))) nil 'silent))))

    (when (and buffer (buffer-live-p buffer))
      (add-to-list 'consult-web--hidden-buffers-list buffer)
      (if sync
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (let* ((end-of-headers (if (and (bound-and-true-p url-http-end-of-headers)
                                              (number-or-marker-p url-http-end-of-headers))
                                         url-http-end-of-headers
                                       (point-min)))
                     (response (buffer-substring (point-min) (pos-eol)))
                     (header (buffer-substring (point-min) end-of-headers))
                     (body (buffer-substring end-of-headers (point-max))))
                (when consult-web-log-level
                  (cond
                   ((eq consult-web-log-level 'info)
                    (consult-web--url-log (format "URL: %s\nRESPONSE: %s" url response)))
                   ((eq consult-web-log-level 'debug)
                    (consult-web--url-log (format "URL: %s\n\nRESPONSE-HEADER:\n%s\n\nRESPONSE-BODY: %s\n" url header body)))))
                (setf response-data (plist-put response-data :status response))
                (delete-region (point-min) (+ end-of-headers 1))
                (goto-char (point-min))
                (if-let* ((parsed-data (condition-case nil
                                     (funcall parser)
                                   (error (funcall error)))))
                    (setf response-data (plist-put response-data :data (funcall callback parsed-data)))))))))
    response-data))

(cl-defun consult-web--request-error-handler (&rest args &key symbol-status error-thrown &allow-other-keys)
  "Handles errors for request backend.
See `request' for more details."
  (message "consult-web: <request>  %s - %s" symbol-status error-thrown))

  (cl-defun consult-web--request-sync (url &rest args &key params headers data parser placeholder error encoding &allow-other-keys)
    "Convinient wrapper for `request'.

Passes all the arguments to request and fetches the
results *synchronously*.

Refer to `request' documents for details."
    (unless (functionp 'request)
      (error "Request backend not available. Either install the package “emacs-request” or change the custom variable `consult-web-retrieve-backend'"))
    (let (candidates)
      (request
        url
        :sync t
        :params params
        :headers headers
        :parser parser
        :error (or error #'consult-web--request-error-handler)
        :data data
        :encoding (or encoding 'utf-8)
        :success (cl-function (lambda (&key data &allow-other-keys)
                                (setq candidates data))))

      candidates))

(cl-defun consult-web--plz-error-handler (plz-error &rest args)
  "Handles errors for `plz' backend.
Refer to `plz' documentation for more details."
  (message "consult-web: <plz> %s" plz-error))

(defun consult-web--json-parse-buffer ()
"Default json parser used in consult-web"
(let ((end-of-headers (if (and (bound-and-true-p url-http-end-of-headers)
                               (number-or-marker-p url-http-end-of-headers))
                          url-http-end-of-headers
                        (point-min))))
(goto-char end-of-headers)
(json-parse-buffer :object-type 'hash-table :array-type 'list :false-object :false :null-object :null)))

(cl-defun consult-web--fetch-url (url backend &rest args &key type params headers data parser callback error encoding timeout sync &allow-other-keys)
  "Retrieves URL with support for different BACKENDs.

This is a wrapper that passes the args to corresponding
BACKEND functions. (i.e. `consult-web-url-queue-retrieve',
 `request', `plz', ...) See backend functions for details.

if SYNC is non-nil, it retrieves URL sunchronously.

TYPE is the http request type (e.g. “GET”, “POST”)

PARAMS are parameters added to the base url
using `consult-web--make-url-string'.

HEADERS are headers passed to headers (e.g. `url-request-extra-headers').

DATA are http request data passed to data (e.g. `url-request-data').

PARSER is a function that is executed in the url-retrieve
response and the results are passed to CALLBACK.
See `consult-web-url-queue-retrieve', `request', or `plz' for more info.

CALLBACK is the function that is executed when the request is complete.
It takes one argument, PARSED-DATA which is the output of the PARSER above.
(i.e. it is called like (funcall CALLBACK (funcall PARSER)))
See `consult-web-url-queue-retrieve', `request', or `plz' for more info.

ERROR is a function that handles errors. It is called without any arguments
in the response buffer.

ENCODING is the encoding used for the request backend (e.g. 'utf-8)

TIMEOUT is the time in seconds for timing out synchronous requests.
This is ignored in async requests.

"
  (cond
   ((eq backend 'plz)
    (if sync
        (funcall callback (funcall #'plz (or type 'get) (consult-web--make-url-string url params)
                                   :headers headers
                                   :as parser
                                   :then 'sync
                                   :else (or error #'consult-web--plz-error-handler)
                                   :timeout (or timeout consult-web-default-timeout)))
      (funcall #'plz (or type 'get) (consult-web--make-url-string url params)
               :headers headers
               :as parser
               :then callback
               :else (or error #'consult-web--plz-error-handler)
               :timeout (or timeout consult-web-default-timeout))))
   ((eq backend 'url)
    (if sync
        (consult-web--url-response-body
         (funcall #'consult-web-url-queue-retrieve url
                  :sync sync
                  :type (or type "GET")
                  :params params
                  :headers headers
                  :parser parser
                  :data data
                  :error (or error #'consult-web--url-retrieve-error-handler)
                  :callback (or callback #'identity)
                  :timeout (or timeout consult-web-default-timeout)))
      (funcall #'consult-web-url-queue-retrieve url
               :sync sync
               :type (or type "GET")
               :params params
               :headers headers
               :parser parser
               :data data
               :error (or error #'consult-web--url-retrieve-error-handler)
               :callback (or callback #'identity)
               :timeout (or timeout consult-web-default-timeout))))
   ((eq backend 'request)
    (if sync
        (funcall callback
                 (request-response-data
                  (funcall #'request url
                           :sync sync
                           :params params
                           :headers headers
                           :parser parser
                           :data data
                           :error (or error #'consult-web--request-error-handler)
                           :encoding (or encoding 'utf-8)
                           :timeout (or timeout consult-web-default-timeout)
                           )))
      (funcall #'request url
               :params params
               :headers headers
               :parser parser
               :data data
               :error (or error #'consult-web--request-error-handler)
               :encoding (or encoding 'utf-8)
               :timeout (or timeout consult-web-default-timeout)
               :complete (cl-function (lambda (&key data &allow-other-keys)
                                        (funcall (or callback #'identity) data)))
               ))
    )))

(defun consult-web--kill-hidden-buffers ()
"Kill all open preview buffers stored in
`consult-gh--preview-buffers-list'.

It asks for confirmation if the buffer is modified
and removes the buffers that are killed from the list."
  (interactive)
  (when consult-web--hidden-buffers-list
    (mapcar (lambda (buff) (if (and (buffer-live-p buff) (not (get-buffer-process buff)))
                             (kill-buffer buff))) consult-web--hidden-buffers-list)
    )
  (setq consult-web--hidden-buffers-list nil)
)

(defun consult-web--kill-url-dead-buffers ()
"Kill buffers in `url-dead-buffer-list'."
  (interactive)
  (when url-dead-buffer-list
    (mapcar (lambda (buff) (if  (and (buffer-live-p buff) (not (get-buffer-process buff)))
                             (kill-buffer buff))
               ) url-dead-buffer-list)
    )
  (setq url-dead-buffer-list nil)
)

(defun consult-web--async-log (formatted &rest args)
  "Log FORMATTED ARGS to variable `consult-web--async-log-buffer'."
  (with-current-buffer (get-buffer-create consult-web--async-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format formatted args))))

(defun consult-web--get-source-prop (source prop)
"Get PROP for SOURCE from `consult-web-sources-alist'."
(plist-get (cdr (assoc source consult-web-sources-alist)) prop)
)

(defun consult-web-dynamic--split-thingatpt (thing &optional split-initial)
  "Return THING at point.

If SPLIT-INITIAL is non-nil, use `consult--async-split-initial' to format the string."
  (when-let (str (thing-at-point thing t))
    (if split-initial
        (consult--async-split-initial str)
      str)))

(cl-defun consult-web--simple-format-candidate (&rest args &key source query url search-url title snippet &allow-other-keys)
  "Returns a simple formatted string for candidates.

SOURCE is the name string of the source for candidate

QUERY is the query string used for searching

URL is a string pointing to url of the candidate

SEARCH-URL is a string pointing to the url for
the search results of QUERY on the SOURCE website

TITLE is the title of the candidate

SNIPPET is a string containing a snippet/description of candidate
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (title-str (consult-web--set-string-width title (* 5 frame-width-percent))))
         (concat title-str
                      (when source (concat "\t" source)))))

(cl-defun consult-web--highlight-format-candidate (&rest args &key source query url search-url title snippet face &allow-other-keys)
  "Returns a highlighted formatted string for candidates.

SOURCE is the name string of the source for candidate

QUERY is the query string used for searching

URL is a string pointing to url of the candidate

SEARCH-URL is a string pointing to the url for
the search results of QUERY on the SOURCE website

TITLE is the title of the candidate

SNIPPET is a string containing a snippet/description of candidate
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-web-source-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-web--get-source-prop source :face) face 'consult-web-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-web--set-string-width title-str (* 4 frame-width-percent)))
         (snippet (and (stringp snippet) (consult-web--set-string-width snippet (* 3 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-web-snippet-face)))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (url-domain urlobj)))
         (domain (and (stringp domain) (propertize domain 'face 'consult-web-domain-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path) (propertize path 'face 'consult-web-path-face)))
         (url-str (consult-web--set-url-width domain path (* frame-width-percent 2)))
         (str (concat title-str
                      (when url-str (concat "\s" url-str))
                      (when snippet (concat "\s\s" snippet))
                      (when source (concat "\t" source)))))
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defun consult-web--group-function (sources cand transform &optional group-by)
  "Group candidates by GROUP-BY keyword.

This is passed as GROUP to `consult--read' on candidates and is used to define the grouping for CAND. "
  (if transform (substring cand)
    (let* ((group-by (or consult-web--override-group-by group-by consult-web-group-by))
           (group-by (if (not (keywordp group-by)) (intern (concat ":" (format "%s" group-by))) group-by)))
      (cond
       ((equal group-by :domain)
        (if-let* ((url (get-text-property 0 :url cand))
                  (urlobj (if url (url-generic-parse-url url) nil))
                  (domain (if (url-p urlobj) (url-domain urlobj))))
            domain
          nil))
       ((member group-by '(:nil :none :no :not))
        nil)
       (group-by
        (if-let ((group (get-text-property 0 group-by cand)))
            (format "%s" group)
          "N/A"))
       (t
        (if-let* ((source (plist-get (consult--multi-source sources cand) :name)))
            source
          nil)))
      )))

(defun consult-web--add-history (&rest args)
  (delq nil
        (cl-remove-duplicates
         (append (mapcar (lambda (thing) (consult-web-dynamic--split-thingatpt thing nil))
                         (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)) (list isearch-string)))))

(defun consult-web--lookup-function ()
"Lookup function for `consult-web' minibuffer candidates.

This is passed as LOOKUP to `consult--read' on candidates and is used to format the output when a candidate is selected."
  (lambda (sel cands &rest args)
     (let* ((info (or (car (member sel cands)) ""))
            (title (get-text-property 0 :title info))
            (url (get-text-property 0 :url info))
            )
      (consult-web-propertize-by-plist (or title url "nil") (or (text-properties-at 0 info) (list)))
      )))

(defun consult-web--default-url-preview (cand)
"Default function to use for previewing CAND."
(when-let* ((url (cond
                  ((listp cand)
                   (or (get-text-property 0 :url (car cand)) (car cand)))
                  (t
                   (or (get-text-property 0 :url cand) cand))))
            (buff (funcall consult-web-default-preview-function url)))
               (funcall (consult--buffer-preview) 'preview
                        buff
                        )
               )
)

(cl-defun consult-web--make-state-function (&rest args &key setup preview exit return &allow-other-keys)
"Convinient wrapper for `consult-web' to make custom state functions.

This can be passed as STATE to `consult--read' on candidates and is
used to define actions when setting up, previewing or selecting a
candidate. Refer to `consult--read' documentation for more details."
    (lambda (action cand &rest args)
      (if cand
          (pcase action
            ('setup
             (funcall setup cand))
            ('preview
             (funcall preview cand))
            ('exit
             (funcall exit cand))
            ('return
             (funcall return cand))
             )))
      )

(defun consult-web--dynamic-state-function ()
  "State function for `consult-web' minibuffer candidates.

This is passed as STATE to `consult--read' on candidates and is used
to define actions that happen when a candidate is previewed or
selected.
The preview and retrun actions are retrieve from `consult-web-sources-alist'."
  (let ((buffer-preview (consult--buffer-preview)))
    (lambda (action cand &rest args)
      (if cand
          (let* ((source (get-text-property 0 :source cand))
                 (state (plist-get (cdr (assoc source consult-web-sources-alist)) :state))
                 (preview (plist-get (cdr (assoc source consult-web-sources-alist)) :on-preview))
                 (return (plist-get (cdr (assoc source consult-web-sources-alist)) :on-return)))
            (if state
                (funcall state action cand args)
              (pcase action
                ('exit
                 (unless consult-web-log-level
                   (consult-web--kill-hidden-buffers)
                   (consult-web--kill-url-dead-buffers)
                   )
                 (funcall buffer-preview 'exit cand))
                ('preview
                 (if preview (funcall preview cand) (consult-web--default-url-preview cand)))
                ('return
                 (if return (funcall return cand) cand)))
              ))))))

(defun consult-web--default-callback (cand)
"Default CALLBACK for CAND.

The CALLBACK is called when a CAND is selected.
When making consult-web sources, if a CALLBACK is not provided, this
CALLBACK is used as a fall back."
  (if-let ((url (get-text-property 0 :url cand)))
      (funcall consult-web-default-browse-function url)))

(defun consult-web--read-search-string (&optional initial)
"Read a string from the minibuffer.

This is used for static commands, when
`consult-web-default-autosuggest-command' is nil."
  (consult--read nil
                 :prompt "Search: "
                 :initial initial
                 :category 'consult-web
                 :history 'consult-web--search-history
                 :add-history (consult-web--add-history)
                                        ))

(defun consult-web--extract-opt-pair (opt opts ignore-opts)
  "Extracts a pair of (OPT . value) from a list OPTS.

values is the next element after OPT in OPTS.
Excludes keys in IGNORE_OPTS.
This i suseful for example to extract key value pairs
from command-line options in alist of strings"
  (unless (member opt ignore-opts)
    (let* ((key (cond
                 ((string-match "--.*$" opt)
                  (intern (concat ":" (replace-regexp-in-string "--" "" opt))))
                 ((string-match ":.*$" opt)
                  (intern opt))
                 (t nil)))
           (val (or (cadr (member opt opts)) "nil"))
           (val (if (stringp val)
                    (intern val))))
    (when key
      (cons key val)))))

(defun consult-web--split-command (input &rest args)
  "Return command argument and options list given INPUT str.

It constructs built-in arguments for count and page, ..., and
it also sets `consult-web--override-group-by' if and argument
for grouping is provided in options.
"
  (pcase-let* ((`(,query . ,opts) (consult--command-split input))
               (args (or args (list)))
               )
    (if (and opts (listp opts) (> (length opts) 0))
        (progn
          (setq opts (cl-substitute ":count" "-n" opts :test 'equal))
          (setq opts (cl-substitute ":page" "-p" opts :test 'equal))
          (setq opts (cl-substitute ":group" "-g" opts :test 'equal))
          (setq opts (cl-substitute ":group" "--group" opts :test 'equal))
          (if (member ":group" opts)
              (setq consult-web--override-group-by (cadr (member ":group" opts)))
          (setq consult-web--override-group-by nil))
          (cl-loop for opt in opts
                   do
                   (pcase-let* ((`(,key . ,val) (consult-web--extract-opt-pair opt opts (list ":group"))))
                     (when key
                       (setq args (append args (list key val)))))))
      (setq consult-web--override-group-by nil))
    (list (or query input) args)
    ))

(defun consult-web--match-minibuffer-content-p (cand)
  "Filter minibuffer candidates by minibuffer content.

Uses regexp to only keep candidates that match
the current content of the minibuffer. This is useful
in turning when using a sync source in an async/dynamic
fashion as the input in the minibuffer is used to filter
the candidates for the sync source
"
  (let* ((win (active-minibuffer-window))
        (buffer (window-buffer win))
        (split-char (plist-get (consult--async-split-style) :initial)))
  (with-current-buffer buffer
    (if (minibuffer-window-active-p win)
        (string-match (concat ".*" (string-trim (car-safe (consult-web--split-command (minibuffer-contents-no-properties))) split-char "\n") ".*") (substring-no-properties cand))))))

(defun consult-web--multi-candidates-static (sources &optional input &rest args)
  "Return `consult--multi' candidates from SOURCES."
  (let* ((candidates (make-vector (length sources) nil))
         (current)
         (idx 0))
    (seq-doseq (src sources)
      (let* ((name (and (plist-member src :name) (plist-get src :name)))
             (face (and (plist-member src :face) `(face ,(plist-get src :face))))
             (cat (plist-get src :category))
             (items (plist-get src :items))
             (narrow (plist-get src :narrow))
             (async-type (and name (consult-web--get-source-prop name :type)))
             (narrow-type (or (car-safe narrow) narrow -1))
             (err (if consult-web-log-level 'err nil))
             )
        (when (or (eq consult--narrow narrow-type)
                  (not (or consult--narrow (plist-get src :hidden))))
          (condition-case err
              (progn
                (when (functionp items)
                  (cond
                   ((and (integerp (cdr (func-arity items))) (< (cdr (func-arity items)) 1))
                    (setq items (funcall items))
                    (aset candidates idx
                          (and items (consult-web--multi-propertize
                                      items cat idx face))))
                   ((eq async-type 'sync)
                    (setq items (funcall items input))
                    (aset candidates idx
                          (and items (consult-web--multi-propertize
                                      items cat idx face))))
                   ((eq async-type 'async)
                    (if input (funcall items input
                                       :callback (lambda (response-items)
                                                   (if response-items
                                                       (setq current
                                                             (and response-items (consult-web--multi-propertize
                                                                                  response-items cat idx face)))
                                                     (setq current t)))
                                       args))
                    (let ((count 0)
                          (max consult-web-default-timeout)
                          (step 0.05))

                      (while (and (< count max) (not current))
                        (+ count step)
                        (if (>= count max)
                            (message "consult-web: Hmmm! %s took longer than expected." (plist-get src :name))
                          (sit-for step)))

                      (aset candidates idx current)))
                   (t
                    (message "source %s needs a :type keyword. See the documentation for `consult-web-define-source'." name)
                    ))))
            ('wrong-type-argument nil)
            ('error
             (message (if consult-web-log-level
                          (format "error in calling :items of %s source - %s" name (error-message-string err))
                        (format "error in calling :items of %s source" name)))
             nil)
            )))
      (cl-incf idx)
      (setq current nil))
    (apply #'append (append candidates nil))))

(defun consult-web--multi-static (sources input args &rest options)
  (let* ((sources (consult--multi-enabled-sources sources))
         (candidates (consult--slow-operation "Give me a few seconds. The internet is a big mess!" (consult-web--multi-candidates-static sources input args)))
         (selected
          (apply #'consult--read
                 candidates
                 (append
                  options
                  (list
                   :sort        nil
                   :history     'consult-web--selection-history
                   :category    'multi-category
                   :predicate   (apply-partially #'consult-web--multi-predicate sources)
                   :annotate    (apply-partially #'consult-web--multi-annotate sources)
                   :group       (apply-partially #'consult-web--multi-group sources)
                   :lookup      (apply-partially #'consult-web--multi-lookup sources)
                   :preview-key (consult--multi-preview-key sources)
                   :narrow      (consult--multi-narrow sources)
                   :state       (consult--multi-state sources))))))
    (if (plist-member (cdr selected) :match)
        (when-let (fun (plist-get (cdr selected) :new))
          (funcall fun (car selected))
          (plist-put (cdr selected) :match 'new))
      (when-let (fun (plist-get (cdr selected) :action))
        (funcall fun (car selected)))
      (setq selected `(,(car selected) :match t ,@(cdr selected))))
    selected))

(defun consult-web--multi-lookup (sources selected candidates _input narrow &rest _)
  "Lookup SELECTED in CANDIDATES given SOURCES, with potential NARROW."
  (if (or (string-blank-p selected)
          (not (consult--tofu-p (aref selected (1- (length selected))))))
      ;; Non-existing candidate without Tofu or default submitted (empty string)
      (let* ((src (cond
                   (narrow (seq-find (lambda (src)
                                       (let ((n (plist-get src :narrow)))
                                         (eq (or (car-safe n) n -1) narrow)))
                                     sources))
                   ((seq-find (lambda (src) (plist-get src :default)) sources))
                   ((seq-find (lambda (src) (not (plist-get src :hidden))) sources))
                   ((aref sources 0))))
             (idx (seq-position sources src))
             (def (and (string-blank-p selected) ;; default candidate
                       (seq-find (lambda (cand) (eq idx (consult--tofu-get cand))) candidates))))
        (if def
            (cons (cdr (get-text-property 0 'multi-category def)) src)
          `(,selected :match nil ,@src)))
    (let* ((found (member selected candidates))
           (info (if found (or (car found) "") ""))
           (title (get-text-property 0 :title info))
           (url (get-text-property 0 :url info))
           )
      (if found
        ;; Existing candidate submitted
        (cons (apply #'propertize (or title url "nil") (or (text-properties-at 0 info) (list)))
              (consult--multi-source sources selected))
      ;; Non-existing Tofu'ed candidate submitted, e.g., via Embark
      `(,(substring selected 0 -1) :match nil ,@(consult--multi-source sources selected))))))

(defun consult-web--multi-group (sources cand transform)
  "Return group string of candidate CAND.

Returns the group string for candidate or transforms it
for all the candidates given SOURCES."
  (if transform
      cand
    (let* ((fun (and (plist-member (consult--multi-source sources cand) :group)
                     (plist-get (consult--multi-source sources cand) :group))))
      (cond
       ((functionp fun)
        (let ((argnum (cdr (func-arity fun))))
        (cond
         ((or (stringp argnum) (and (numberp argnum) (> argnum 2)))
              (funcall fun sources cand transform))
         ((and (numberp argnum) (= argnum 2))
          (funcall fun cand transform))
         ((and (numberp argnum) (= argnum 1))
          (funcall fun cand)))))
       ((stringp fun)
        fun)
       ((eq fun 'nil)
        nil)
       (t
        (plist-get (consult--multi-source sources cand) :name))))))

(defun consult-web--multi-predicate (sources cand)
  "Predicate function called for each candidate CAND given SOURCES."
  (let* ((src (consult--multi-source sources cand))
         (narrow (plist-get src :narrow))
         (type (or (car-safe narrow) narrow -1))
         (pred (plist-get src :predicate))
         (show t)
         )
    (if pred
        (cond
         ((booleanp pred)
          (setq show pred))
         ((and (functionp pred) (> (car (func-arity pred)) 0))
          (setq show (funcall pred cand)))))
      (and show
           (or (eq consult--narrow type)
               (not (or consult--narrow (plist-get src :hidden)))))))

(defun consult-web--multi-enabled-sources (sources)
  "Return vector of enabled SOURCES."
  (vconcat
   (seq-filter (lambda (src)
                 (if-let (pred (plist-get src :enabled))
                     (cond
                      ((functionp pred)
                       (funcall pred))
                      (t
                       pred))
                   t))
               (mapcar (lambda (src)
                         (if (symbolp src) (symbol-value src) src))
                       sources))))

(defun consult-web--multi-propertize (response-items category pos &optional face)
  "Propertize RESPONSE-ITEMS with the multi-category datum and FACE.

POS and CATEGORY are the group ID and category for these items."
  (let ((annotated-items))
    (dolist (item response-items annotated-items)
      (if (consp item) (setq item (or (car-safe item) item)))
      (let* ((cand (consult--tofu-append item pos)))
        ;; Preserve existing `multi-category' datum of the candidate.
        (if (get-text-property 0 'multi-category cand)
            (when face (add-text-properties 0 (length item) face cand))
          ;; Attach `multi-category' datum and face.
          (add-text-properties 0 (length item)
                               `(multi-category (,category . ,item) ,@face) cand))
        (push cand annotated-items)))))

(defun consult-web--multi-annotate (sources cand)
  "Annotate candidate CAND from multi SOURCES."
  (let ((src (consult--multi-source sources cand)))
    (if-let ((fun (plist-get src :annotate)))
        (cond
         ((functionp fun)
          (funcall fun (cdr (get-text-property 0 'multi-category cand))))
         ((and (symbolp fun) (functionp (eval fun)))
          (funcall (eval fun) (cdr (get-text-property 0 'multi-category cand))))))
    ))

(defun consult-web--multi-update-sync-candidates (async source idx action &rest args)
  (let* ((face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (cat (plist-get source :category))
         (fun (plist-get source :items))
         (items))
    (when (functionp fun)
      (cond
       ((and (integerp (cdr (func-arity fun))) (< (cdr (func-arity fun)) 1))
        (setq items (funcall fun)))
       (t
        (setq items (funcall fun action args)))))
    (funcall async (and items (consult-web--multi-propertize items cat idx face)))
    (funcall async 'refresh)
))

(defun consult-web--multi-update-dynamic-candidates (async source idx action &rest args)
  (let* ((face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (cat (plist-get source :category)))
    (funcall (plist-get source :items) action
             :callback (lambda (response-items)
                         (when response-items
                           (funcall async (consult-web--multi-propertize response-items cat idx face))
                           (funcall async 'refresh)
                           )) args)))

(defun consult-web--multi-update-async-candidates (async source idx action &rest args)
  ""
  (let* ((name (plist-get source :name))
         (builder (plist-get source :items))
         (transform (consult-web--get-source-prop name :transform))
         (props (seq-drop-while (lambda (x) (not (keywordp x))) args))
         (proc)
         (proc-buf)
         (count)
         (face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (consult-web--async-log-buffer (concat " *consult-web-async-log--" name "*"))
         (cat (plist-get source :category))
         (query (car (consult-web--split-command action)))
         (args (funcall builder action)))
    (unless (stringp (car args))
        (setq args (car args)))
      (when proc
        (delete-process proc)
        (kill-buffer proc-buf)
        (setq proc nil proc-buf nil))
      (when args
        (let* ((rest "")
               (proc-filter
                (lambda (_ out)
                  (let ((lines (split-string out "[\r\n]+")))
                    (if (not (cdr lines))
                        (setq rest (concat rest (car lines)))
                      (setcar lines (concat rest (car lines)))
                      (let* ((len (length lines))
                             (last (nthcdr (- len 2) lines)))
                        (setq rest (cadr last)
                              count (+ count len -1))
                        (setcdr last nil)
                        (when lines
                          (setq lines (mapcar (lambda (line) (propertize line :source name :title line :query query)) lines))
                          (when transform (setq lines (funcall transform lines query)))
                          (funcall async (consult-web--multi-propertize lines cat idx face))
                          (funcall async 'refresh))
                        )))))
               (proc-sentinel
                (lambda (_ event)
                  (funcall async 'indicator
                           (cond
                            ((string-prefix-p "killed" event)   'killed)
                            ((string-prefix-p "finished" event) 'finished)
                            (t 'failed)))
                  (when (and (string-prefix-p "finished" event) (not (equal rest "")))
                    (cl-incf count)
                    (funcall async (list rest)))
                  (consult-web--async-log
                   "consult--async-process sentinel: event=%s lines=%d\n"
                   (string-trim event) count)
                  (when (> (buffer-size proc-buf) 0)
                    (with-current-buffer (get-buffer-create consult-web--async-log-buffer)
                      (goto-char (point-max))
                      (insert ">>>>> stderr >>>>>\n")
                      (let ((beg (point)))
                        (insert-buffer-substring proc-buf)
                        (save-excursion
                          (goto-char beg)
                          (message #("%s" 0 2 (face error))
                                   (buffer-substring-no-properties (pos-bol) (pos-eol)))))
                      (insert "<<<<< stderr <<<<<\n")))))
               (process-adaptive-read-buffering nil))
          (funcall async 'indicator 'running)
          (consult-web--async-log "consult--async-process started %S\n" args)
          (setq count 0
                proc-buf (generate-new-buffer (concat " *consult-web-async-stderr-" name "*"))
                proc (apply #'make-process
                            `(,@props
                              :connection-type pipe
                              :name ,(car args)
                              ;; :stderr ,proc-buf
                              :process-buffer ,proc-buf
                              :noquery t
                              :command ,args
                              :filter ,proc-filter
                              :sentinel ,proc-sentinel)))))
    (when proc (add-to-list 'consult-web-async-processes `(,proc . ,proc-buf)))))

(defun consult-web--multi-cancel ()
  (mapcar (lambda (proc) (when proc (delete-process (car proc))
                               (kill-buffer (cdr proc))
                               ))
          consult-web-async-processes)
  (setq consult-web-async-processes nil)
  (mapcar (lambda (timer) (when timer (cancel-timer timer))) consult-web-dynamic-timers)
  (setq consult-web-dynamic-timers nil))

(defun consult-web--multi-update-candidates (async sources action &rest args)
  "Dynamically updates CANDIDATES for multiple SOURCES

ASYNC is the sink function
SOURCES are sources
INPUT is the input string to pass to SOURCES
"
  (let ((idx 0))
    (seq-doseq (src sources)
      (let* ((name (plist-get src :name))
             (items (plist-get src :items))
             (narrow (plist-get src :narrow))
             (async-type (consult-web--get-source-prop name :type))
             (narrow-type (or (car-safe narrow) narrow -1))
             (err (if consult-web-log-level 'err nil)))
        (when (or (eq consult--narrow narrow-type)
                  (not (or consult--narrow (plist-get src :hidden))))
          (condition-case err
              (progn
                (when (functionp items)
                  (cond
                   (; sync source, append candidates right away
                    (equal async-type 'sync)
                    (consult-web--multi-update-sync-candidates async src idx action args)
                    )
                   (; async source, append candidatesin process
                    (equal async-type 'async)
                    (consult-web--multi-update-async-candidates async src idx action args)
                    )
                    (; dynamic source, append candidates in a callback function
                     (equal async-type 'dynamic)
                     (consult-web--multi-update-dynamic-candidates async src idx action args)

                     )
                    (t
                     (message "source %s needs a :type keyword. See the documentation for `consult-web-define-source'." name
                              )))
                   ))
                ('error ;; message other erros
                 (funcall async 'indicator 'killed)
                 (message (if consult-web-log-level
                              (format "error in calling :items of %s source - %s" name (error-message-string err))
                            (format "error in calling :items of %s source" name)))
                 nil)
                )))
        (cl-incf idx))
      ))

(defun consult-web--multi-dynamic-collection (async sources &rest args)
  "Dynamic computation of candidates.
ASYNC is the sink
SOURCES is list of sources to use
"
  (setq async (consult--async-indicator async))
  (let ((consult-web-async-processes (list))
        (consult-web-dynamic-timers (list))
        (current))
    (lambda (action)
      (pcase action
        ('nil
         (funcall async nil))
        (""
         (setq current nil)
         (consult-web--multi-cancel)
         (funcall async 'flush)
         (funcall async 'indicator 'finished)
         )
        ((pred stringp)
         (if (equal action current)
             (funcall async 'indicator 'finished)
           (progn
             (setq current action)
             (consult-web--multi-update-candidates async sources action args)
             (funcall async 'refresh))))
        ('destroy
         (consult-web--multi-cancel)
         (funcall async 'destroy))
        (_ (funcall async action))))))

(defun consult-web--multi-dynamic-command (sources &rest args)
"Dynamic collection with input splitting on multiple SOURCES."
(declare (indent 1))
(thread-first
  (consult--async-sink)
  (consult--async-refresh-timer)
  (consult-web--multi-dynamic-collection sources args)
  (consult--async-throttle)
  (consult--async-split)))

(cl-defun consult-web--multi-dynamic (sources args &rest options)
"Select candidates with dynamic input from a list of SOURCES.

This is similar to `consult--multi'
but accepts async/dynamic sources as well.
See `consult--multi' for more info.
"
 (let* ((sources (consult-web--multi-enabled-sources sources))
         (selected
          (apply #'consult--read
                 (consult-web--multi-dynamic-command sources args)
                 (append
                  options
                  (list
                   :sort        nil
                   :history     '(:input consult-web--search-history)
                   :initial     (consult--async-split-initial nil)
                   :category    'multi-category
                   :predicate   (apply-partially #'consult-web--multi-predicate sources)
                   :annotate    (apply-partially #'consult-web--multi-annotate sources)
                   :group       (apply-partially #'consult-web--multi-group sources)
                   :lookup      (apply-partially #'consult-web--multi-lookup sources)
                   :preview-key (consult--multi-preview-key sources)
                   :narrow      (consult--multi-narrow sources)
                   :state       (consult--multi-state sources))))))
    (if (plist-member (cdr selected) :match)
        (when-let (fun (plist-get (cdr selected) :new))
          (funcall fun (car selected))
          (plist-put (cdr selected) :match 'new))
      (when-let (fun (plist-get (cdr selected) :action))
        (funcall fun (car selected)))
      (setq selected `(,(car selected) :match t ,@(cdr selected))))
    selected))

(defun consult-web--source-name (source-name &optional suffix)
  "Returns a symbol for SOURCE-NAME variable.

The variable is consult-web--source-%s (%s=source-name).
Adds suffix to the name if provided."
  (intern (format "consult-web--source-%s" (concat (replace-regexp-in-string " " "-" (downcase source-name)) (if suffix (downcase suffix) nil)))))

(defun consult-web--source-generate-docstring (source-name)
  "Makes a generic documentation string for SOURCE-NAME.

This is used in `consult-web-define-source' macro to make generic
docstrings for variables."
  (format "consult-web source for %s.\n \nThis function was defined by the macro `consult-web-define-source'."
          (capitalize source-name)))

(defun consult-web--func-name (source-name &optional prefix suffix)
  "Make a function symbol for interactive command for SOURCE-NAME.

Adds prefix and suffix if non-nil."
  (intern (concat "consult-web-" (if prefix prefix) (replace-regexp-in-string " " "-" (downcase source-name)) (if suffix suffix))))

(defun consult-web--func-generate-docstring (source-name &optional dynamic)
  "Make a generic documentaion string for an interactive command.

This is used to make docstring for function made by `consult-web-define-source'."
  (concat "consult-web's " (if dynamic "dynamic ") (format "interactive command to search %s."
                                                             (capitalize source-name))))

(defun consult-web--make-source-list (source-name request annotate face narrow-char state preview-key category lookup group sort enabled predicate selection-history)
  "Internal function to make a source for `consult--multi'.

Do not use this function directly, use `consult-web-define-source' macro
instead."
  `(:name ,source-name
          ,(when (and annotate face) :face)
          ,(when (and annotate face) (cond
            ((eq face t)
             'consult-web-default-face)
            (t face)))
          :narrow ,narrow-char
          :state ,(or state #'consult-web--dynamic-state-function)
          :category ,(or category 'consult-web)
          :history ,selection-history
          :add-history (delq nil
                                    (cl-remove-duplicates
                                     (append (mapcar (lambda (thing) (consult-web-dynamic--split-thingatpt thing))
                                             (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)) (list isearch-string))))
          :items  ,request
          :annotate ,(cond
                      ((and annotate (functionp annotate))
                       annotate)
                      ((eq annotate t)
                       #'consult-web--annotate-function)
                      (t nil))
          :lookup ,(if (and lookup (functionp lookup))
                      lookup
                    #'consult-web--lookup-function)
          :group ,(or group #'consult-web--group-function)
          :preview-key ,(and consult-web-show-preview (or preview-key consult-web-preview-key))
          ,(when enabled ':enabled)
          ,(when enabled enabled)
          :sort ,sort
          ,(when predicate ':predicate)
          ,(when predicate predicate)
          ))

(defun consult-web--call-static-command (input no-callback args request face state source-name category lookup selection-history-var annotate preview-key sort)
  "Internal function to make static `consult--read' command.

Do not use this function directly, use `consult-web-define-source' macro
instead."
  (let* ((input (or input
                    (and consult-web-default-autosuggest-command (funcall-interactively consult-web-default-autosuggest-command))
                    (consult-web--read-search-string)))

         (prompt (concat "[" (propertize (format "%s" (consult-web--func-name source-name)) 'face 'consult-web-prompt-face) "]" " Search: "))
         (selected (consult-web--multi-static (list (consult-web--source-name source-name))
                                              input
                                              args
                                              :prompt prompt
                                              :sort sort
                                              :history selection-history-var))
         (selected (cond
                    ((consp selected) (car-safe selected))
                    (t selected)))
         (source (get-text-property 0 :source selected))
         )
    (unless no-callback
      (if source
          (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)))
    selected)
  )

(defun consult-web--call-dynamic-command (initial no-callback args source-name request category face lookup search-history-var selection-history-var preview-key sort)
  "Internal function to make dynamic `consult--read' command.

Do not use this function directly, use `consult-web-define-source' macro
 instead."
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (prompt (concat "[" (propertize (format "%s" (consult-web--func-name source-name)) 'face 'consult-web-prompt-face) "]" " Search: "))
         (selected (consult-web--multi-dynamic (list (consult-web--source-name source-name))
                                               args
                                      :prompt prompt
                                      :history '(:input search-history-var)
                                      :initial (consult--async-split-initial initial)
                                      :sort sort
                                      ))
         (selected (cond
                    ((consp selected) (car selected))
                    (t selected)))
         (source (get-text-property 0 :source selected))
         (title (get-text-property 0 :title selected)))
    (add-to-history selection-history-var title)
    (unless no-callback
      (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)
      )
    selected
    ))

;;; Macros
;;;###autoload
(cl-defmacro consult-web-define-source (source-name &rest args &key type request transform on-preview on-return state on-callback lookup dynamic group narrow-char category search-history selection-history face annotate preview-key docstring enabled sort predicate &allow-other-keys)
  "Macro to make a consult-web-source for SOURCE-NAME.

\* Makes
- source for `consult-web-multi' and/or `consult-web-dynamic'
- interactive commands (static or dynamic) for single source
- adds a new row to to `consult-web-sources-alist' with all the
metadata as a property list.

\* Keyword Arguments

Brief Description:

==========  ==========      =================================================
Keyword     Type            Explanation
==========  ==========      =================================================

TYPE        (sync|async)    Whether the source is synchronous or asynchronous

REQUEST     (function)      Fetch results from source

ON-PREVIEW  (function)      Preview action in `consult--read'

ON-RETURN   (function)      Return action in `consult--read'

STATE       (function)      STATE passed to `consult--read'
                            (bypasses ON-PREVIEW and ON-RETURN)

ON-CALLBACK (function)      Function called on selected candidate

DYNAMIC     (boolean|'both) Whether to make dynamic or non-dynamic commands

GROUP       (function)      Passed as GROUP to `consult--read'

ANNOTATE    (function)      Passed as ANNOTATE to `consult--read'

NARROW-CHAR (char)          Passed as NARROW to `consult-read'

CATEGORY    (symbol)        Passed as CATEGORY to `consult--read'

HISTORY     (symbol)        Passed as HISTORY to `consult--read'

SORT        (boolean)       Passed as SORT to `consult--read'

ENABLED     (function)      Passed as ENABLED to `consult--read'

PREDICATE   (function)      Passed as PREDICATE to `consult--read'

FACE        (face)          Passed as FACE to `consult--read-multi'

PREVIEW-KEY (key)           Passed as PREVIEW-KEY to `consult--read'

DOCSTRING   (string)        DOCSTRING for the variable created for SOURCE-NAME

===================================================================

Detailed Decription:

TYPE can be either 'sync or 'async, depending on how the items for the source
should be collected.'sync sources can readily get their candidates
in a synchronous process (i.e. a function that returns a list). This can be dyanmic
(meaning a funciton that takes an input argument) or static (i.e. a funciton without
 any arguments).
'async sources need an async process to collectb their candidates
(e.g. require loading a url asynchronously). In this case the list of candidates arrive
at different times and the minibuffer candidates need to be updated in an interval.

REQUEST is a function that returns the list of candidates.
The recommended format is to use cl-defun and accept keyword arguments
with this signature:

For synchronous sources REQEUEST can take 0 (for static sources), or 1 input argument
(for dynamic sources) and return a list of candidates.

For synchronous sources, REQUEST should take at least one input argument
as well as a keyword argument called callback. The input argument is the string
from the user input in the minibuffer. Body of the function builds the list of candidates
and passes it to callback. The format should look like this:

(cl-defun consult-web--elfeed-fetch-results (input &rest args &key callback &allow-other-keys)
BODY
(funcall callback candidates)
)

Examples can be found in the wiki pages of the repo or in
“consult-web-sources.el” on the repository webpage or :
URL `https://github.com/armindarvish/consult-web/blob/main/consult-web-sources.el'


ON-PREVIEW is used as a function to call on the candidate, when a preview is
requested. It takes one required argument, the candidate. For an example,
see `consult-web-default-preview-function'.

ON-RETURN is used as a function to call on the candidate, when the
candidate is selected. This is passed to consult built-in state
function machinery.
Note that the output of this function will be returned in the consult-web
commands. In consult-web, ON-CALLBACK is used to call further actions on
this returned value. This allows to separate the return value from the
commands and the action that i run on the selected candidates. Therefore
for most use cases, ON-RETURN can just be `#'identity' to get
the candidate back as it is. But if some transformation is needed,
ON-RETURN can be used to transform the selected candidate.


STATE is a function that takes no argument and returns a function for
consult--read STATE argument. For an example see
`consult-web--dynamic-state-function' that builds state function based on
 ON-PREVIEW and ON-RETURN. If STATE is non-nil, instead of using
ON-PREVIEW and ON-RETURN to make a state function, STATE will be directly
used in consult--read.


ON-CALLBACK is the function that is called with one required input argument,
 the selected candidate. For example, see `consult-web--default-callback'
that opens the url of the candidate in the default browser.
Other examples can be found in the wiki pages of the repo or in
“consult-web-sources.el” on the repository webpage or :
URL `https://github.com/armindarvish/consult-web/blob/main/consult-web-sources.el'

DYNAMIC can be a bollean (nil or t) or the symbol 'both.
If nil only \*non-dynamic\* interactive commands are created in this macro.
if t only \*dynamic\* interactive commands are created in this macro.
If something else (e.g. 'both) \*Both\* dynamic and non-dynamic commands
are created.

GROUP, ANNOTATE, NARROW-CHAR, CATEGORY, and PREVIEW-KEY are passed to
`consult--read'. See consult's Documentaion for more
 details.

FACE can be used to format the candidate.

DOCSTRING is used as docstring for the variable consult-web--source-%s
variable that this macro creates for %s=SOURCE-NAME.
"
  (if (symbolp source-name) (setq source-name (eval source-name)))

  `(progn

     ;; make a variable called consult-web--source-%s (%s=source-name)
     (defvar ,(consult-web--source-name source-name) nil)
     (setq ,(consult-web--source-name source-name) (consult-web--make-source-list ,source-name ,request ,annotate ,face ,narrow-char ,state ,preview-key ,category ,lookup ,group ,sort ,enabled ,predicate ,selection-history))
      ;; make a dynamic interactive command consult-web-dynamic-%s (%s=source-name)
     (if ,dynamic
         (defun ,(consult-web--func-name source-name) (&optional initial no-callback &rest args)
           ,(or docstring (consult-web--func-generate-docstring source-name t))
           (interactive "P")
           (consult-web--call-dynamic-command initial no-callback args ,source-name ,request ,category ,face ,lookup ,search-history ,selection-history ,preview-key ,sort)
           ))

     ;; make a static interactive command consult-web-%s (%s=source-name)
     (unless (eq ,dynamic t)
       (defun ,(consult-web--func-name source-name nil "-static") (&optional input no-callback &rest args)
         ,(or docstring (consult-web--func-generate-docstring source-name))
         (interactive "P")
         (consult-web--call-static-command input no-callback args ,request ,face ,state ,source-name ,category ,lookup ,selection-history ,annotate ,preview-key ,sort)
         ))

     ;; add source to consult-web-sources-alist
     (add-to-list 'consult-web-sources-alist (cons ,source-name
                                                          (list :name ,source-name
                                                                :type ,type
                                                                :source (consult-web--source-name ,source-name)
                                                                :face ,face
                                                                :request-func ,request
                                                                :transform ,transform
                                                                :on-preview (or ,on-preview #'consult-web--default-url-preview)
                                                                :on-return (or ,on-return #'identity)
                                                                :on-callback (or ,on-callback #'consult-web--default-callback)
                                                                :state ,state
                                                                :group ,group
                                                                :annotate ,annotate
                                                                :narrow-char ,narrow-char
                                                                :preview-key ,preview-key
                                                                :category (or ',category 'consult-web)
                                                                :search-history ,search-history
                                                                :selection-history ,selection-history
                                                                :interactive-static (and (functionp (consult-web--func-name ,source-name)) (consult-web--func-name ,source-name))
                                                                :interactive-dynamic (and (functionp (consult-web--func-name ,source-name "dynamic-")) (consult-web--func-name ,source-name "dynamic-"))
                                                                :enabled ,enabled
                                                                :sort ,sort
                                                                :predicate ,predicate
                                                                )))

     ,source-name))

;;;###autoload
(cl-defmacro consult-web--make-fetch-function (source &rest args &key source-name docstring &allow-other-keys)
  "Make a function for fetching result based on SOURCE.

SOURCE is a source for consult (e.g. a plist that is passed
to consult--multi). See `consult-buffer-sources' for examples.
SOURCE-NAME is a string name for SOURCE
DOCSTRING is the docstring for the function that is returned."
  (let* ((source (if (plistp source) source (eval source)))
        (source-name (substring-no-properties (plist-get source :name))))
  `(progn
     ;; make a function that creates a consult--read source for consult-web-multi
     (cl-defun ,(consult-web--source-name source-name "-fetch-results") (input &rest args &key callback &allow-other-keys)
       ,(or docstring (consult-web--source-generate-docstring source-name))
  (pcase-let* ((`(,query . ,opts) (consult-web--split-command input))
         (opts (car-safe opts))
         (fun  (plist-get ',source :items))
         (results (cond
                   ((functionp fun) (funcall fun))
                   ((listp fun) fun)
                   ))
         (source (substring-no-properties (plist-get ',source :name))))
    (delq nil (mapcar (lambda (item)
                        (if (consp item) (setq item (or (car-safe item) item)))
              (when (string-match (concat ".*" query ".*") item)
                  (propertize item
                              :source source
                              :title item
                              :url nil
                              :query query
                              :search-url nil
                              ))) results)))))))

(cl-defun consult-web--make-source-from-consult-source (consult-source &rest args &key type request on-preview on-return state on-callback group narrow-char category dynamic search-history selection-history face annotate enabled sort predicate preview-key docstring &allow-other-keys)
"Makes a consult-web source from a consult source, CONSULT-SOURCE.

All othe input variables are passed to `consult-web-define-source'
macro. See `consult-web-define-source' for more details"
  (if (boundp consult-source)
        (let* ((source (eval consult-source))
               (source (if (plistp source) source (eval source)))
               (name (and (plistp source) (substring-no-properties (plist-get source :name))))
               (narrow-char (or narrow-char (and (plistp source) (plist-get source :narrow))))
               (narrow-char (if (listp narrow-char) (car narrow-char)))
               (face (or face (and (plistp source) (plist-get source :face))))
               (state (or state (and (plistp source) (plist-get source :state))))
               (annotate (cond
                          ((eq annotate 'nil) nil)
                          ((eq annotate 't) (and (plistp source) (plist-get source :annotate)))
                          (t annotate)))
               (preview-key (or preview-key (and (plistp source) (plist-get source :preview-key)) consult-web-preview-key))
               (predicate (or predicate (and (plistp source) (plist-get source :predicate))))
               (group (or group (and (plistp source) (plist-get source :group))))
               (sort (or sort (and (plistp source) (plist-get source :sort))))
               (enabled (or enabled (and (plistp source) (plist-get source :enabled))))
               (category (or category (and (plistp source) (plist-get source :category)) 'consult-web)))
          (eval (macroexpand
           `(consult-web-define-source ,name
                                     :docstring ,docstring
                                     :narrow-char ,narrow-char
                                     :type ',type
                                     :face ',face
                                     :category ',category
                                     :request (or ,request (consult-web--make-fetch-function ,source))
                                     :preview-key ,preview-key
                                     :search-history ',search-history
                                     :selection-history ',selection-history
                                     :on-preview ',on-preview
                                     :on-return ',on-return
                                     :on-callback ',on-callback
                                     :enabled ',enabled
                                     :predicate ',predicate
                                     :group ',group
                                     :sort ',sort
                                     :dynamic ',dynamic
                                     :annotate ',annotate
                                     ))))
    (display-warning :warning (format "Consult-web: %s is not available. Make sure `consult-notes' is loaded and set up properly" consult-source)))
  )

(defun consult-web-multi (&optional initial sources no-callback &rest args)
  "Interactive “multi-source dynamic search”

INITIAL is the initial search prompt in minibuffer.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-dynamic-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.

This is an interactive command that fetches results form all the sources in `consult-web-dynamic-sources' with dynamic completion meaning that
the search term can be dynamically updated by the user
and the results are fetched as the user types in the miinibuffer.

Additional commandline arguments can be passed in the minibuffer
entry similar to `consult-grep' by typing `--` followed by arguments.

For example the user can enter:

`#consult-web -- -g domain'

this will run a search on all the `consult-web-dynamic-sources' for
the term “consult-web” and then groups the results by the “domain
of the URL” of the results.

Built-in arguments include:

 -g, --groups, or :groups  for grouping (see `consult-web-group-by' and `consult-web--override-group-by'. for more info)

 -n, --count, or :count is passed as the value for COUNT
to any source in `consult-web-dynamic-sources'.

 -p, --page, or :page is passed as the value for PAGE to any source
 in `consult-web-dynamic-sources'.

Custom arguments can be passed by using “--ARG value” (or “:ARG value”).
For example, if the user types the following in the minibuffer:
“#how to do web search in emacs? -- --model gpt-4”
The term “how to do web search in emacs?” is passed as the search
term and the “gpt-4” as a keyword argument for :model to every
source in `consult-web-dynamic-sources'. If any request function of
the sources takes a keyword argument for :model, “gpt-4” is
used then.

Once the results are fetched, narrowing down can be done by
using consult-split-style syntax (e.g. “#” for “perl” style)
after the serach term, similar to `consult-grep'.
For example:
“#consult-web#github.com”
uses “consult-web” as the search term, and then narrows the choices to
results that have “github.com” in them.

For more examples, refer to the official documentation of the repo here:
URL `https://github.com/armindarvish/consult-web'.

For more details on consult--async functionalities, you can also
see `consult-grep' and the official manual of consult,
here: URL `https://github.com/minad/consult'."
  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-dynamic-sources))
         (sources (remove nil (mapcar (lambda (source) (plist-get (cdr (assoc source consult-web-sources-alist)) :source)) sources)))
         (prompt (concat "[" (propertize "consult-web-multi" 'face 'consult-web-prompt-face) "]" " Search:  "))
         (selected
          (car-safe (consult-web--multi-dynamic
                     sources
                     args
                     :prompt prompt
                     :sort t
                     :history '(:input consult-web--search-history)
                     :initial (consult--async-split-initial initial)
                     )))
         (source (get-text-property 0 :source selected)))
    (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)
    selected
    ))

(defun consult-web-static (&optional input sources no-callback &rest args)
  "Interactive “static” multi-source search

INPUT is the initial search query. Searches all sources
in SOURCES for INPUT.
If SOURCES is nil, `consult-web-static-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned
without any callback action.
"
  (interactive "P")
  (let* ((input (or input
                    (and consult-web-default-autosuggest-command (funcall-interactively consult-web-default-autosuggest-command))
                    (consult-web--read-search-string)))
         (sources (or sources consult-web-static-sources))
         (sources (remove nil (mapcar (lambda (source) (plist-get (cdr (assoc source consult-web-sources-alist)) :source)) sources)))
         (prompt (concat "[" (propertize "consult-web-sync" 'face 'consult-web-prompt-face) "]" " Search:  "))
         (selected
          (car-safe (consult-web--multi-static sources
                                               input
                                               nil
                                               :prompt prompt
                                               :history 'consult-web--selection-history
                                               :sort t
                                               )))
         (source (get-text-property 0 :source selected)))
        (unless no-callback
          (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected))
    selected
    ))

(defun consult-web-scholar (&optional initial sources no-callback &rest args)
  "Interactive “multi-source acadmic literature” search

INITIAL is the initial search prompt in minibuffer.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-scholar-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.

This is similar to `consult-web-multi', but runs the search on academic literature sources in `consult-web-scholar-sources'.
Refer to `consult-web-multi' for more details."
  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-scholar-sources))
         (sources (remove nil (mapcar (lambda (source) (plist-get (cdr (assoc source consult-web-sources-alist)) :source)) sources)))
         (prompt (concat "[" (propertize "consult-web-scholar" 'face 'consult-web-prompt-face) "]" " Search:  "))
         (selected
          (car-safe (consult-web--multi-dynamic
                     sources
                     args
                     :prompt prompt
                     :sort t
                     :history '(:input consult-web--search-history)
                     :initial (consult--async-split-initial initial)
                     )))
         (source (get-text-property 0 :source selected)))
    (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)
    selected
    ))

(defun consult-web-omni (&optional initial sources no-callback &rest args)
  "Interactive “multi-source and dynamic omni search”
This is for using combination of web and local sources defined in
`consult-web-omni-sources'.

INITIAL is the initial search prompt in minibuffer.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-omni-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.

This is a dynamic command and additional arguments can be passed in
the minibuffer. See `consult-web-multi' for more details."

  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-omni-sources))
         (sources (remove nil (mapcar (lambda (source) (plist-get (cdr (assoc source consult-web-sources-alist)) :source)) sources)))
         (prompt (concat "[" (propertize "consult-web-omni" 'face 'consult-web-prompt-face) "]" " Search:  "))
         (selected
          (car-safe (consult-web--multi-dynamic
                     sources
                     args
                     :prompt prompt
                     :sort t
                     :history '(:input consult-web--search-history)
                     :initial (consult--async-split-initial initial)
                     )))
         (source (get-text-property 0 :source selected)))
    (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)
    selected
    ))

(defun consult-web (&rest args)
"Wrapper function that calls the function in `consult-web-default-interactive-command'.

This is for conviniece to call the favorite consult-web interactive command."
  (interactive)
  (apply consult-web-default-interactive-command args))

;;; provide `consult-web' module

(provide 'consult-web)

;;; consult-web.el ends here
