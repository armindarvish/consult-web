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
  (require 'consult)
  (require 'url)
  (when (featurep 'json)
    (require 'json))
  (when (featurep 'request)
    (require 'request))
  )

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
  "Default timeout in seconds for
`consult-web--url-retrieve-synchronously."
  :type 'integer)

(defcustom consult-web-log nil
  "Default timeout in seconds for
`consult-web--url-retrieve-synchronously."
  :type 'boolean)

(defcustom consult-web-log-buffer-name " *consult-web-log*"
"String for consult-web-log buffer name"
:type 'string)

(defcustom consult-web-log-level nil
  "How to make logs for consult-web requests?
This can be set to nil, info or debug
nil: Does not log anything
info: Logs urls and response's http header
debug: Logs urls and the entire http response.

When non-nil, information is logged to `consult-web-log-buffer-name'."
  :type '(choice
          (const :tag "No Logging" nil)
          (const :tag "Just HTTP Header" info)
          (const :tag "Full Response" debug)))

(defcustom consult-web-group-by :source
  "What field to use to group the results in the minibuffer?

By default it is set to :domain. but can be any of:

  :url      group by URL
  :domain   group by the domain of the URL
  :source   group by source
 "
  :type '(radio (const :tag "url path" :url)
                (const :tag "domain of url path":domain)
                (const :tag "name of the search engine or source" :source)
                (const :tag "custom other field (constant)" :any)
                (const :tag "do not group" nil)))


(defcustom consult-web-multi-sources  (list)
  "List of sources used by `consult-web-multi'.

This variable is a list of strings, each string being name of a source.
The source name has to be a key from `consult-web-sources-alist'.
Sources can be made with the convinient macro `consult-web-define-source'."
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

(defcustom consult-web-dynamic-omni-sources  (list)
"List of sources used by `consult-web-dynamic-omni'.

This variable is a list of strings, each string being name of a source.
The source name has to be a key from `consult-web-sources-alist'.
Sources can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-source-from-consult-source'."
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-scholar-sources  (list)
  "List of sources used by `consult-web-scholar'.

This variable is a list of strings, each string being name of a source.
The source name has to be a key from `consult-web-sources-alist'.
Sources can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-source-from-consult-source'."
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-dynamic-sources  (list)
  "List of sources used by `consult-web-dynamic'.

This variable is a list of strings, each string being name of a source.
The source name has to be a key from `consult-web-sources-alist'.
Sources can be made with the convinient macro `consult-web-define-source'
or by using `consult-web--make-source-from-consult-source'."
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-web-highlight-matches t
  "Should `consult-web' highlight search queries in the minibuffer?"
  :type 'boolean)


(defcustom consult-web-default-interactive-command #'consult-web-multi
  "Which command should `consult-web' call?"
  :type '(choice (function :tag "(Default) Search with dynamic completion (i.e. `consult-web-dynamic')" #'consult-web-dynamic)
                 (function :tag "Search without dynamic completion (i.e. `consult-web-multi')"  #'consult-web-multi)
                 (function :tag "Search academic research literature (i.e. `consult-web-scholar')"  #'consult-web-scholar)
                 (function :tag "Custom function")))

(defcustom consult-web-retrieve-backend #'consult-web-url-retrieve-sync
  "Which command should `consult-web' use for url requests?"
  :type '(choice (function :tag "(Default) url-retrieve backend" #'consult-web-url-retrieve-sync)
                 (function :tag "Emacs Request Backend"  #'consult-web--request-sync)))

(defcustom consult-web-http-retrieve-backend 'url
  "Which command should `consult-web' use for url requests?"
  :type   '(choice
          (const :tag "(Default) Built-in Emacs's url-retrive" 'url)
          (const :tag "`request' backend" 'request)
          (const :tag "`plz' backend" 'plz)))

(defcustom consult-web-default-autosuggest-command nil
  "Which command should `consult-web' use for auto suggestion on search input?"
  :type '(choice (function :tag "(default) use brave autosuggestion (i.e. `consult-web-brave-autosuggest')" #'consult-web-brave-autosuggest)
                 (function :tag "use google autosuggestion (i.e. `consult-web-dynamic-google-autosuggest')" #'consult-web-dynamic-google-autosuggest)
                 (function :tag "custom function")))

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
  "Category symbol for the `consult-web' package.")

(defvar consult-web-scholar-category 'consult-web-scholar
  "Category symbol for the `consult-web' package.")

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

(defvar consult-web--current-sources (list)
"List of sources of the candidates in the current minibuffer.

This is used for defining narrow functions
(e.g. `consult-web--dynamic-narrow-function'."
)

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
  `((t :inherit 'font-lock-string-face))
"The face for academic literature source types in minibuffer.")

(defface consult-web-domain-face
  `((t :inherit 'font-lock-variable-face))
"The face for domain annotation in minibuffer.")

(defface consult-web-path-face
  `((t :inherit 'font-lock-warning-face))
"The face for path annotation in minibuffer.")

(defface consult-web-snippet-face
  `((t :inherit 'font-lock-doc-face))
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

(defun consult-web--set-string-width (string width &optional prepend)
  "Sets the STRING width to a fixed value, WIDTH.

If the STRING is longer than WIDTH, it truncates the STRING
 and adds ellipsis, \"...\". if the STRING is shorter,
it adds whitespace to the STRING.
If PREPEND is non-nil, it truncates or adds whitespace from
 the beginning of STRING, instead of the end."
  (let* ((string (format "%s" string))
         (w (string-width string)))
    (when (< w width)
      (if prepend
          (setq string (format "%s%s" (consult-web-propertize-by-plist (make-string (- width w) ?\s) (text-properties-at 0 string)) (substring string)))
        (setq string (format "%s%s" (substring string) (consult-web-propertize-by-plist (make-string (- width w) ?\s) (text-properties-at 0 (substring string -1)))))))
    (when (> w width)
      (if prepend
          (setq string (format "%s%s" (consult-web-propertize-by-plist "..." (text-properties-at 0 string)) (substring string (- w (- width 3)) w)))
        (setq string (format "%s%s" (substring string 0 (- width (+ w 3))) (consult-web-propertize-by-plist "..." (text-properties-at 0 (substring string (- width (+ w 3)) )))))))
    string))

(defun consult-web--justify-left (string prefix maxwidth)
  "Sets the width of STRING+PREFIX justified from left.
It uses `consult-web--set-string-width' and sets the width
 of the concatenate of STRING+PREFIX
(e.g. `(concat PREFIX STRING)`) within MAXWIDTH.
This is used for aligning marginalia info in minibuffer."
  (let ((s (string-width string))
        (w (string-width prefix)))
    (if (> maxwidth w)
    (consult-web--set-string-width string (- maxwidth w) t)
    string
          )
    ))

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

(defun consult-web--log (string)
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

(cl-defun consult-web--url-retrieve-synchronously (url &rest settings &key params headers parser callback data (type "GET") error (encoding 'utf-8) timeout)
"Retrieves URL synchronously.

Passes all the arguments to url-retriev and fetches the results.

PARAMS are parameters added to the base url using `consult-web--make-url-string'.
HEADERS are headers passed to `url-request-extra-headers'.
DATA are http request data passed to `url-request-data'.
TYPE is the http request type (e.g. “GET”, “POST”)
ERROR
ENCODING
TIMEOUT
PARSER is a function that is executed in the url-retrieve response buffer and the results are returned s the output of this function.
"
  (let* ((url-request-method type)
         (url-request-extra-headers headers)
         (url-request-data data)
         (url-with-params (consult-web--make-url-string url params))
         (url-debug (if consult-web-log-level t nil))
         (response-data nil)
         (buffer (if timeout
                     (with-timeout
                         (timeout
                          (setf response-data (plist-put response-data :status 'timeout))
                          nil)
                       (url-retrieve-synchronously url-with-params t))
                   (url-retrieve-synchronously url-with-params t))
                 ))

    (when buffer
      (add-to-list 'consult-web--hidden-buffers-list buffer)
      (with-current-buffer buffer
        (when consult-web-log-level
          (save-excursion
            (goto-char (point-min))
            (cond
             ((eq consult-web-log-level 'info)
              (consult-web--log (format "URL: %s\nRESPONSE: %s" url (buffer-substring (point-min) (pos-eol)))))
             ((eq consult-web-log-level 'debug)
                 (consult-web--log (format "URL: %s\n\nRESPONSE-HEADER:\n%s\n\nRESPONSE-BODY: %s\n" url (buffer-substring (point-min) url-http-end-of-headers) (buffer-substring url-http-end-of-headers (point-max))))))
            ))

        (let* ((response-header (buffer-substring (point-min) url-http-end-of-headers))
               (response-content (buffer-substring (+ url-http-end-of-headers 1) (point-max)))
               (response-status (consult-web--parse-http-response))
               )
          (delete-region (point-min) (+ url-http-end-of-headers 1))

          (when-let ((parsed-data (funcall parser)))
            (setf response-data (plist-put response-data :data parsed-data))
            )

          (when response-header
            (setf response-data (plist-put response-data :header response-header)))

          (when response-status
            (setf response-data (plist-put response-data :status response-status)))

          (when response-content
            (setf response-data (plist-put response-data :content response-content)))

          )))
    response-data
    ))

(cl-defun consult-web-url-retrieve-sync (url &key params headers parser data (type "GET") error (encoding 'utf-8) timeout)
"Retrieves URL synchronously.

Passes all the arguments to `consult-web--url-retrieve-synchronously'
and in trun to `url-retrieve' to fetch the results synchronously.

TYPE is the http request type (e.g. “GET”, “POST”)
PARAMS are parameters added to the base url using `consult-web--make-url-string'.
HEADERS are headers passed to headers (e.g. `url-request-extra-headers').
DATA are http request data passed to data (e.g. `url-request-data').
PARSER is a function that is executed in the url-retrieve
response buffer and the results are returned s the output of this function.
CALLBACK is the function that is execute when the request is complete.
ERROR is a function that handles errors
ENCODING is the encoding used for the request (e.g. 'utf-8)
TIMEOUT is the time in seconds for timing out the request
"
    (consult-web--url-response-body
     (consult-web--url-retrieve-synchronously url
                                              :params params
                                              :headers headers
                                              :parser parser
                                              :data data
                                              :type type
                                              :error error
                                              :encoding encoding
                                              :timeout (or timeout consult-web-default-timeout))))

(cl-defun consult-web-url-retrieve-async (url &rest settings &key params headers parser data (type "GET") callback error (encoding 'utf-8) timeout)
"Retrieves URL synchronously.

Passes all the arguments to url-retrieve
and fetches the results asynchronously.

TYPE is the http request type (e.g. “GET”, “POST”)

PARAMS are parameters added to the base url
using `consult-web--make-url-string'.

HEADERS are headers passed to headers (e.g. `url-request-extra-headers').

DATA are http request data passed to data (e.g. `url-request-data').

PARSER is a function that is executed in the url-retrieve
response buffer and the results are returned s the output of this function.

CALLBACK is the function that is executed when the request is complete.

ERROR is a function that handles errors.

ENCODING is the encoding used for the request (e.g. 'utf-8).

TIMEOUT is the time in seconds for timing out the request.
"
  (let* ((url-request-method type)
         (url-request-extra-headers headers)
         (url-request-data data)
         (url-with-params (consult-web--make-url-string url params))
         (url-debug (if consult-web-log-level t nil))
         (response-data nil)
         (buffer (if timeout
                     (with-timeout
                         (timeout
                          (setf response-data (plist-put response-data :status 'timeout))
                          nil)
                       (url-retrieve url-with-params
                                     (lambda (_)
                                       (when-let* ((attrs (condition-case nil
                                                              (funcall parser)
                                                            (error (funcall error)))))
                                                   (funcall callback attrs))) nil 'silent))
                   (url-retrieve url-with-params
                                 (lambda (_) (let* ((attrs (condition-case nil
                                                              (funcall parser)
                                                            (error (funcall error)))))
                                               (funcall callback attrs))) nil 'silent))
                 ))

    (when buffer
      (add-to-list 'consult-web--hidden-buffers-list buffer)
      (with-current-buffer buffer
        (when consult-web-log-level
          (save-excursion
            (goto-char (point-min))
            (cond
             ((eq consult-web-log-level 'info)
              (consult-web--log (format "URL: %s\nRESPONSE: %s" url (buffer-substring (point-min) (pos-eol)))))
             ((eq consult-web-log-level 'debug)
                 (consult-web--log (format "URL: %s\n\nRESPONSE-HEADER:\n%s\n\nRESPONSE-BODY: %s\n" url (buffer-substring (point-min) url-http-end-of-headers) (buffer-substring url-http-end-of-headers (point-max))))))
            ))
        )
    )))

(cl-defun consult-web-url-retrieve (url &rest settings &key params headers parser data (sync 'nil) (type "GET") callback error (encoding 'utf-8) timeout)
  "Retrieves URL synchronously.

Passes all the arguments to url-retrieve
and fetches the results asynchronously.

TYPE is the http request type (e.g. “GET”, “POST”)

PARAMS are parameters added to the base url
using `consult-web--make-url-string'.

HEADERS are headers passed to headers (e.g. `url-request-extra-headers').

DATA are http request data passed to data (e.g. `url-request-data').

PARSER is a function that is executed in the url-retrieve
response buffer and the results are returned s the output of this function.

CALLBACK is the function that is executed when the request is complete.

ERROR is a function that handles errors.

ENCODING is the encoding used for the request (e.g. 'utf-8).

TIMEOUT is the time in seconds for timing out the request.
"
  (let* ((url-request-method type)
         (url-request-extra-headers headers)
         (url-request-data data)
         (url-with-params (consult-web--make-url-string url params))
         (url-debug (if consult-web-log-level t nil))
         (response-data nil)
         (buffer (if timeout
                     (with-timeout
                         (timeout
                          (setf response-data (plist-put response-data :status 'timeout))
                          nil)
                       (if sync
                           (url-retrieve-synchronously url-with-params nil 'silent)
                         (url-retrieve url-with-params
                                       (lambda (_)
                                         (when-let* ((attrs (condition-case nil
                                                                (funcall parser)
                                                              (error (funcall error)))))
                                           (funcall callback attrs))) nil 'silent)))
                   (if sync
                       (url-retrieve-synchronously url-with-params nil 'silent)
                     (url-retrieve url-with-params
                                   (lambda (_) (let* ((attrs (condition-case nil
                                                                 (funcall parser)
                                                               (error (funcall error)))))
                                                 (funcall callback attrs))) nil 'silent)))))
    (when buffer
      (add-to-list 'consult-web--hidden-buffers-list buffer)
      (with-current-buffer buffer
        (when consult-web-log-level
          (save-excursion
            (goto-char (point-min))
            (cond
             ((eq consult-web-log-level 'info)
              (consult-web--log (format "URL: %s\nRESPONSE: %s" url (buffer-substring (point-min) (pos-eol)))))
             ((eq consult-web-log-level 'debug)
              (consult-web--log (format "URL: %s\n\nRESPONSE-HEADER:\n%s\n\nRESPONSE-BODY: %s\n" url (buffer-substring (point-min) url-http-end-of-headers) (buffer-substring url-http-end-of-headers (point-max))))))
            ))
        (if (number-or-marker-p url-http-end-of-headers)
            (delete-region (point-min) (+ url-http-end-of-headers 1)))
        (if sync
            (progn
              (goto-char (point-min))
              (if-let* ((attrs (condition-case nil
                                   (funcall parser)
                                 (error (funcall error)))))
                          (setf response-data (plist-put response-data :data (funcall callback attrs))))))))
        response-data))

(cl-defun consult-web--request-error-handler (&rest args &key symbol-status error-thrown &allow-other-keys)
  "Handles errors for request backend."
  (message "consult-web: <request>  %s - %s" symbol-status error-thrown))

  (cl-defun consult-web--request-sync (url &rest args &key params headers data parser placeholder error encoding &allow-other-keys)
    "Convinient wrapper for `request'.

Passes all the arguments to request and fetches the results *synchronously*.

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
  "Handles errors for plz backend."
  (message "consult-web: <plz> %s" plz-error))

(defun consult-web--default-url-parse-buffer ()
""
(let ((end-of-headers (if (and (bound-and-true-p url-http-end-of-headers)
                               (number-or-marker-p url-http-end-of-headers))
                          url-http-end-of-headers
                        (point-min))))
(goto-char end-of-headers)
(json-parse-buffer :object-type 'hash-table :array-type 'list :false-object :false :null-object :null)))

(cl-defun consult-web--fetch-url (url backend &rest args &key type params headers data parser callback error encoding timeout sync &allow-other-keys)
  "Retrieves URL synchronously.

Passes all the arguments to `consult-web--url-retrieve-synchronously' and in trun to `url-retrieve' fetches the results.

TYPE is the http request type (e.g. “GET”, “POST”)
PARAMS are parameters added to the base url using `consult-web--make-url-string'.
HEADERS are headers passed to headers (e.g. `url-request-extra-headers').
DATA are http request data passed to data (e.g. `url-request-data').
PARSER is a function that is executed in the url-retrieve
response buffer and the results are returned s the output of this function.
CALLBACK is the function that is execute when the request is complete.
ERROR is a function that handles errors
ENCODING is the encoding used for the request (e.g. 'utf-8)
TIMEOUT is the time in seconds for timing out the request
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
         (funcall #'consult-web-url-retrieve url
                  :sync sync
                  :type (or type "GET")
                  :params params
                  :headers headers
                  :parser parser
                  :data data
                  :error (or error #'consult-web--url-retrieve-error-handler)
                  :callback (or callback #'identity)
                  :encoding (or encoding 'utf-8)
                  :timeout (or timeout consult-web-default-timeout)))
      (funcall #'consult-web-url-retrieve url
               :sync sync
               :type (or type "GET")
               :params params
               :headers headers
               :parser parser
               :data data
               :error (or error #'consult-web--url-retrieve-error-handler)
               :callback (or callback #'identity)
               :encoding (or encoding 'utf-8)
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

(defun consult-web--kill-dead-buffers ()
"Kill all open preview buffers stored in `consult-gh--preview-buffers-list'.
It asks for confirmation if the buffer is modified and removes the buffers that are killed from the list."
  (interactive)
  (when url-dead-buffer-list
    (mapcar (lambda (buff) (if  (and (buffer-live-p buff) (not (get-buffer-process buff)))
                             (kill-buffer buff))
               ) url-dead-buffer-list)
    )
  (setq url-dead-buffer-list nil)
)

(defun consult-web--get-source-prop (source prop)
(plist-get (cdr (assoc source consult-web-sources-alist)) prop)
)

(defun consult-web-dynamic--split-thingatpt (thing &optional split-initial)
  "Return THING at point.
If SPLIT-INITIAL is non-nil, use `consult--async-split-initial' to format the string."
  (when-let (str (thing-at-point thing t))
    (if split-initial
        (consult--async-split-initial str)
      str)))

(defun consult-web--table-to-formatted-candidate-simple (table &optional face &rest args)
"Returns a formatted candidate for TABLE.

TABLE is a hashtable that stores metadata for a consult-web candidate.
Returns a cons set of `key . value`;
The key is the value of :title key in the TABLE.
The value is all the (key value) pairs in the table as a plist.
"
           (let* ((query (gethash :query table))
                  (title (format "%s" (gethash :title table)))
                  (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
                  (pl (consult-web-hashtable-to-plist table))
                   )
              (apply #'propertize title-str pl)
))

(defun consult-web--table-to-formatted-candidate-searchable (table &optional face &rest args)
"Formats a consult-web candidate.

TABLE is a hashtable with metadata for the candidate as (key value) pairs.
Returns a string (from :title field in TABLE) with text-properties that conatin
all the key value pairs in the table.
"
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
         (snippet (gethash :snippet table))
         (snippet (if (and snippet (stringp snippet) (> (string-width snippet) 25)) (concat (substring snippet 0 22) "...") snippet))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (title-str (consult-web--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or face 'consult-web-default-face)))
         (extra-args (consult-web-hashtable-to-plist table '(:title :url :search-url :query :source :snippet)))
         (str (concat title-str (if domain (concat "\t" domain (if path path))) (if snippet (format "\s\s%s" snippet)) (if source (concat "\t" source)) (if extra-args (format "\s\s%s" extra-args))))
         (str (apply #'propertize str pl))
         )
    (if consult-web-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-web--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-web--highlight-match match-str str t)))))
    str))

(defun consult-web--format-candidates-list (list &optional format-func face)
"Format a LIST of candidates.

LIST is a list of hashtables, each representing one candidate.
FORMAT-FUNC is a function that is used to format candidates if provided.
Returns a list of formatted candidates using either FORMAT-FUNC or otherwise uses default formating for the source retrieved from `consult-web-sources-alist'."
  (mapcar (lambda (table)
            (let* ((source (gethash :source table))
                  (format-func (or format-func
                         (plist-get (cdr (assoc source consult-web-sources-alist)) :format-func)
                         #'consult-web--table-to-formatted-candidate-searchable))
                  (face (or face
                         (plist-get (cdr (assoc source consult-web-sources-alist)) :face)
                         'consult-web-default-face))
                  )
              (funcall format-func table face))) list))

(defun consult-web--annotate-function (cand)
"Annotates each candidate in the minibuffer.

This is provided for convinience to be passed as `:annotate' key when making sources using `consult-web-define-source'.
For more info on annotation refer to `consult' manual, particularly 'consult--read' and `consult--read-annotate' documentation."

    (let* ((url (get-text-property 0 :url cand))
           (urlobj (if url (url-generic-parse-url url)))
           (domain (if (url-p urlobj) (url-domain urlobj) nil))
           (path (if (url-p urlobj) (url-filename urlobj) nil))
           (url-str nil)
           (source (get-text-property 0 :source cand))
           (snippet (get-text-property 0 :snippet cand))
           (extra-args (consult-web-properties-to-plist cand '(:url :source :title :search-url :query :snippet :model :backend))))
      (if domain (setq domain (propertize domain 'face 'consult-web-domain-face)))
      (if path (setq path (propertize path 'face 'consult-web-path-face)))
      (if (and snippet (stringp snippet) (> (string-width snippet) 25)) (setq snippet (concat (substring snippet 0 22) "...")))
      (setq url-str (concat (if domain domain) (if path path)))
      (unless (string-empty-p url-str) (setq url url-str))
      (when (and url (> (string-width url) (floor (* (frame-width) 0.4))))
        (setq url (consult-web--set-string-width url (floor (* (frame-width) 0.4)))))
      (concat (if url (format "\s%s" url)) (if source (format "\t%s" source)) (if snippet (format "\s\s%s" snippet)) (if extra-args (format "\t%s" extra-args)))
    ))

;; (defun consult-web--group-function (group-by cand transform)
;;   "Group candidates by GROUP-BY keyword.

;; This is passed as GROUP to `consult--read' on candidates and is used to define the grouping for CAND. "
;;   (let* ((group-by (or consult-web--override-group-by group-by consult-web-group-by))
;;          (group-by (if (not (keywordp group-by)) (intern (concat ":" (format "%s" group-by))) group-by))
;;          (name (or (if group-by (get-text-property 0 group-by cand) "N/A"))))
;;     (cond
;;      ((equal group-by :domain)
;;       (when-let* ((url (get-text-property 0 :url cand))
;;                   (urlobj (if url (url-generic-parse-url url) nil))
;;                   (domain (if (url-p urlobj) (url-domain urlobj))))
;;         (setq name domain))))
;;   (if transform (substring cand) name)))

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
            group
          "N/A"))
       (t
        (if-let* ((source (plist-get (consult--multi-source sources cand) :name)))
            source
          nil)))
      )))

(defun consult-web--narrow-function (source)
"Make a narrowing (key . value) pair for the SOURCE string.

key is the first character, and value is the entire source STRING.
For example when “wikipedia” is passed as a source, it returns (w . “wikipedia”)."
 `(,(string-to-char source) . ,source)
)

(defun consult-web--dynamic-narrow-function ()
  "Dynamically makes a list of (key . value) for all the sources in the current list of candidates using `consult-web--narrow-function'."
  (let* ((narrow-pred (lambda (cand)
                       (if-let ((source (get-text-property 0 :source (car cand))))
                         (equal (string-to-char source) consult--narrow)
                           )))
        (narrow-keys (mapcar (lambda (c) (cons (string-to-char c) c))
                              consult-web--current-sources)))
`(:Predicate ,narrow-pred :keys ,narrow-keys)
))

(defun consult-web--lookup-function ()
"Lookup function for `consult-web' minibuffer candidates.

This is passed as LOOKUP to `consult--read' on candidates and is used to format the output when a candidate is selected."
  (lambda (sel cands &rest args)
     (let* ((info (or (car (member sel cands)) ""))
            (title (get-text-property 0 :title info))
            (url (get-text-property 0 :url info))
            )
      (apply #'propertize (or title url "nil") (or (text-properties-at 0 info) (list)))
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
                   (consult-web--kill-dead-buffers))
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
  (consult--read nil
                 :prompt "Search: "
                 :initial initial
                 :category 'consult-web
                 :history 'consult-web--search-history
                 :add-history (delq nil
                                    (cl-remove-duplicates
                                     (append (mapcar (lambda (thing) (consult-web-dynamic--split-thingatpt thing nil))
                                             (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)) (list isearch-string))))
                                        ))

(defun consult-web--extract-opt-pair (opt opts ignore-opts)
  "Extracts a pair of (OPT . value) from a list OPTS.

values is the next element after OPT in OPTS.
Excludes keys in IGNORE_OPTS.
This i suseful for example to extract key value pairs
from command-line options in alist of strings"
 (let* ((key (cond
             ((string-match "--.*$" opt)
             (intern (concat ":" (replace-regexp-in-string "--" "" opt))))
             ((string-match ":.*$" opt)
              (intern opt))
             (t nil)))
       (val (or (cadr (member opt opts)) "nil"))
       (val (cond
             ((string-match "--.*$\\|:.*$" val)
              nil)
             ((stringp val)
              (intern val)))))
   (when (and key (not (member opt ignore-opts)))
   (cons key val))
))

(defun consult-web--split-args (args)
  "Splits ARGS to remaining args and input
input is the last element of ARGS
remaining args are turned into a plist"
 (pcase-let* ((input (car (last args)))
              (args (seq-difference (remove input args) '((nil nil) (nil)))) ;;this is hacky should find a better way
              (`(,arg . ,opts) (consult--command-split input))
              (remaining-opts (list)))
    (cl-loop for opt in opts
             do
             (pcase-let* ((`(,key . ,val) (consult-web--extract-opt-pair opt opts (list "--group" ":group"))))

        (when key
          (setq args (append args (list key val)))
          (setq remaining-opts (cl-delete-duplicates (append remaining-opts (list opt (format "%s" val))))))
        ))

    (setq opts (seq-difference opts remaining-opts))

    (when (member "-n" opts)
      (setq args (append args `(:count ,(intern (or (nth (+ (cl-position "-n" opts :test 'equal) 1) opts) "nil"))))))

    (when (member "-p" opts)
      (setq args (append args `(:page ,(intern (or (nth (+ (cl-position "-p" opts :test 'equal) 1) opts) "nil")))))
      )

    (if (or (member "-g" opts) (member ":group" opts) (member "--group" opts))
      (cond
       ((member "-g" opts)
        (setq consult-web--override-group-by (intern (or (nth (+ (cl-position "-g" opts :test 'equal) 1) opts) "nil")))
        )
       ((member "--group" opts)
        (setq consult-web--override-group-by (intern (or (nth (+ (cl-position "--group" opts :test 'equal) 1) opts) "nil")))
        )
       ((member ":group" opts)
        (setq consult-web--override-group-by (intern (or (nth (+ (cl-position ":group" opts :test 'equal) 1) opts) "nil")))
        ))
       (setq consult-web--override-group-by nil)
        )
    (list (or arg input) args)
))

(defun consult-web-dynamic--list-from-sources (sources &optional format-func face &rest args)
  "Builds ARGS from user input and collects candidates from all
SOURCES."
  (pcase-let* ((`(,input ,args) (consult-web--split-args args)))
    (cond
     ((and (listp sources))
      (apply 'append
             (cl-loop for source in sources
                      collect
                      (consult-web--format-candidates-list
                       (apply source input args)))))
     ((functionp sources)
      (consult-web--format-candidates-list
       (apply sources input args) format-func face))
     (t
      (error "%s is not a consult-web-source!")))))

(defun consult-web-dynamic--collection (sources &optional format-func face &rest args)
"This is a wrapper using `consult--dynamic-collection' and
`consult-web-dynamic--list-from-sources'."
(consult--dynamic-collection (apply-partially #'consult-web-dynamic--list-from-sources sources format-func face args)))

(defun consult-web-dynamic--internal (prompt collection &optional initial category lookup history-var preview-key)
"internal function to run `consult--read'.

PROMPT COLLECTION and INITIAL are passed to `consult--read'."
(consult--read collection
                   :prompt prompt
                   :group (apply-partially #'consult-web--group-function :source)
                   :narrow (consult-web--dynamic-narrow-function)
                   :lookup (or lookup (consult-web--lookup-function))
                   :state (consult-web--dynamic-state-function)
                   :initial (consult--async-split-initial initial)
                   :category (or category 'consult-web)
                   :preview-key (and consult-web-show-preview (or preview-key consult-web-preview-key))
                   :history (cond
                             ((eq history-var t)
                              t)
                             ((eq history-var nil)
                              nil)
                             ((and history-var (symbolp history-var))
                              `(:input ,history-var)))
                   :add-history (delq nil
                                    (cl-remove-duplicates
                                     (append (mapcar (lambda (thing) (consult-web-dynamic--split-thingatpt thing t))
                                             (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)) (list isearch-string))))
                   :sort t
                   )
)

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
                   :sort        t
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

(defun consult-web--split-command (input &rest args)
  (pcase-let* ((`(,query . ,opts) (consult--command-split input))
               (remaining-opts (list))
               (args (or args (list)))
               )
    (if opts
        (progn
      (cl-loop for opt in opts
               do
               (pcase-let* ((`(,key . ,val) (consult-web--extract-opt-pair opt opts (list "--group" ":group"))))
                 (when key
                   (setq args (append args (list key val)))
                   (setq remaining-opts (cl-delete-duplicates (append remaining-opts (list opt (format "%s" val))))))
                 ))

      (setq opts (seq-difference opts remaining-opts))

      (when (member "-n" opts)
        (setq args (append args `(:count ,(cadr (member "-n" opts))))))

      (when (member "-p" opts)
        (setq args (append args `(:page ,(cadr (member "-p" opts))))))

      (if (or (member "-g" opts) (member ":group" opts) (member "--group" opts))
          (cond
           ((member "-g" opts)
            (setq consult-web--override-group-by (cadr (member "-g" opts))))
           ((member "--group" opts)
            (setq consult-web--override-group-by (cadr (member "--group" opts))))
           ((member ":group" opts)
            (setq consult-web--override-group-by (cadr (member ":group" opts)))))
        (setq consult-web--override-group-by nil)
        ))
      (setq consult-web--override-group-by nil))
    (list (or query input) args)
    ))

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
  "Return title of candidate CAND or TRANSFORM the candidate given SOURCES."
  (if transform cand
    (let* ((fun (and (plist-member (consult--multi-source sources cand) :group)
                     (plist-get (consult--multi-source sources cand) :group))))
      (cond
       ((functionp fun)
        (funcall fun sources cand transform))
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

(defun consult-web--match-minibuffer-content-p (cand)
  (let* ((win (active-minibuffer-window))
        (buffer (window-buffer win))
        (split-char (plist-get (consult--async-split-style) :initial)))
  (with-current-buffer buffer
    (if (minibuffer-window-active-p win)
        (string-match (concat ".*" (string-trim (car-safe (consult-web--split-command (minibuffer-contents-no-properties))) split-char "\n") ".*") (substring-no-properties cand))))))

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
  (let ((src (consult--multi-source sources cand)))
    (if-let ((fun (plist-get src :annotate)))
        (cond
         ((functionp fun)
          (funcall fun (cdr (get-text-property 0 'multi-category cand))))
         ((and (symbolp fun) (functionp (eval fun)))
          (funcall (eval fun) (cdr (get-text-property 0 'multi-category cand)))))
      )))

(defun cosnult-web--multi-dynamic-candidates-update (async sources candidates input &rest args)
    (let ((idx 0))
      (seq-doseq (src sources)
        (let* ((face (and (plist-member src :face) `(face ,(plist-get src :face))))
               (name (plist-get src :name))
               (cat (plist-get src :category))
               (items (plist-get src :items))
               (narrow (plist-get src :narrow))
               (async-type (consult-web--get-source-prop name :type))
               (narrow-type (or (car-safe narrow) narrow -1))
               (err (if consult-web-log-level 'err nil))
               (pos idx))
          (when (or (eq consult--narrow narrow-type)
                    (not (or consult--narrow (plist-get src :hidden))))
            (condition-case err
                (progn
                  (when (functionp items)
                    (cond
                     ((and (integerp (cdr (func-arity items))) (< (cdr (func-arity items)) 1))
                      (setq items (funcall items))
                      (aset candidates idx    ; sync source, refresh now
                            (and items (consult-web--multi-propertize
                                        items cat idx face)))
                      (funcall async 'flush)
                      (funcall async (apply #'append (append candidates nil)))
                      ;; (funcall async 'refresh)
                      )
                     ((equal async-type 'sync)
                      (setq items (funcall items input args))
                      (aset candidates idx    ; sync source, refresh now
                            (and items (consult-web--multi-propertize
                                        items cat idx face)))
                      (funcall async 'flush)
                      (funcall async (apply #'append (append candidates nil)))
                      ;; (funcall async 'refresh)
                      )
                     ((equal async-type 'async)
                      (if input (funcall items input      ; async source, refresh in callback
                               :callback (lambda (response-items)
                                 (when response-items
                                   (aset candidates pos
                                         (consult-web--multi-propertize response-items cat pos face))
                                   (funcall async 'flush)
                                   (funcall async (apply #'append (append candidates nil)))
                                   (funcall async 'refresh)
                                   )) args)))
                     (t
                    (message "source %s needs a :type keyword. See the documentation for `consult-web-define-source'." name
                   )))
                    ))
              ('wrong-type-argument nil)
              ('error
               (message (if consult-web-log-level
                            (format "error in calling :items of %s source - %s" name (error-message-string err))
                          (format "error in calling :items of %s source" name)))
             nil)
              )))
        (cl-incf idx))
      candidates))

(defun consult-web--multi-dynamic-compute (async sources &rest args)
  "Dynamic computation of candidates.
ASYNC is the sink.
FUN computes the candidates given the input.
DEBOUNCE is the time after which an interrupted computation
should be restarted."
  (setq async (consult--async-indicator async))
  (let* ((request) (current) (timer)
         (debounce consult-web-dynamic-input-debounce)
         (candidates (make-vector (length sources) nil))
         (cancel (lambda () (when timer (cancel-timer timer) (setq timer nil))))
         (start (lambda (req) (setq request req) (funcall async 'refresh)))
         (fun (apply-partially #'cosnult-web--multi-dynamic-candidates-update async sources))
         )
    (lambda (action)
      (pcase action
        ((and 'nil (guard (not request)))
         (funcall async nil))
        ('nil
         (funcall cancel)
         (let ((state 'killed))
           (unwind-protect
               (progn
                 (funcall async 'indicator 'running)
                 (redisplay)
                 ;; Run computation
                 (let* ((response (funcall fun candidates request args)))
                   ;; Flush and update candidate list
                       (setq candidates response)
                       (if (or (equal response 'nil) (equal response [nil]))
                           (funcall async 'flush)
                         (funcall async 'nil)
                         )
                       (setq state 'finished
                         current request)
                     ))
             (funcall async 'indicator state)
             ;; If the computation was killed, restart it after some time.
             (when (eq state 'killed)
               (setq timer (run-at-time debounce nil start request)))
             (setq request nil))))
        ((pred stringp)
         (funcall cancel)
         (if (or (equal action "") (equal action current))
               (funcall async 'indicator 'finished)
           (funcall start action)
           ))
        ('destroy
         (funcall cancel)
         (funcall async 'destroy))
        (_ (funcall async action))))))

(defun consult-web--async-sink ()
  "Create ASYNC sink function.

An async function must accept a single action argument.  For the
\\='setup action it is guaranteed that the call originates from
the minibuffer.  For the other actions no assumption about the
context can be made.

\\='setup   Setup the internal closure state.  Return nil.
\\='destroy Destroy the internal closure state.  Return nil.
\\='flush   Flush the list of candidates.  Return nil.
\\='refresh Request UI refresh.  Return nil.
nil      Return the list of candidates.
list     Append the list to the already existing candidates list and return it.
string   Update with the current user input string.  Return nil."
  (let (candidates last buffer)
    (lambda (action)
      (pcase-exhaustive action
        ('setup
         (setq buffer (current-buffer))
         nil)
        ((or (pred stringp) 'destroy) nil)
        ('flush (setq candidates nil last nil))
        ('refresh
         ;; Refresh the UI when the current minibuffer window belongs
         ;; to the current asynchronous completion session.
         (when-let (win (active-minibuffer-window))
           (when (eq (window-buffer win) buffer)
             (with-selected-window win
               (run-hooks 'consult--completion-refresh-hook)
               ;; Interaction between asynchronous completion functions and
               ;; preview: We have to trigger preview immediately when
               ;; candidates arrive (gh:minad/consult#436).
               (when (and consult--preview-function candidates)
                 (funcall consult--preview-function)))))
         nil)
        ('nil candidates)
        ((pred consp)
         (setq last (last (if last (setcdr last action) (setq candidates action))))
         candidates)))))

(defun consult-web--multi-async (async sources)
  "Merge the results of (a)sync SOURCES and pass it to function ASYNC."
  (let ((candidates (make-vector (length sources) nil)))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (unless (equal action "")
           (let ((idx 0))
             (seq-doseq (src sources)
               (let* ((face (and (plist-member src :face) `(face ,(plist-get src :face))))
                      (cat (plist-get src :category))
                      (items (plist-get src :items))
                      (narrow (plist-get src :narrow))
                      (type (or (car-safe narrow) narrow -1))
                      (pos idx))
                 (when (or (eq consult--narrow type)
                           (not (or consult--narrow (plist-get src :hidden))))
                   (condition-case nil
                       (progn
                         (when (functionp items)
                           (cond
                            ((and (integerp (cdr (func-arity items))) (< (cdr (func-arity items)) 1))
                             (setq items (funcall items))
                             (aset candidates idx    ; sync source, refresh now
                               (and items (consult-web--multi-propertize
                                           items cat idx face)))
                             (funcall async 'flush)
                             (funcall async (apply #'append (append candidates nil))))
                            ((and (integerp (cdr (func-arity items))) (< (cdr (func-arity items)) 2))
                             (setq items (funcall items action))
                             (aset candidates idx    ; sync source, refresh now
                                   (and items (consult-web--multi-propertize
                                               items cat idx face)))
                             (funcall async 'flush)
                             (funcall async (apply #'append (append candidates nil))))
                            ((and (integerp (cdr (func-arity items))) (< (cdr (func-arity items)) 3))
                             (funcall items action      ; async source, refresh in callback
                               (lambda (response-items)
                                 (when response-items
                                   (aset candidates pos
                                         (consult-web--multi-propertize response-items cat pos face))
                                   (funcall async 'flush)
                                   (funcall async (apply #'append (append candidates nil))))))))
                            ))
                     (t
                      (message "calling :items in %s source produced error" src))
                     )))
               (cl-incf idx)))))
        (_ (funcall async action))))))

(defun consult-web--multi-dynamic-collection (sources &rest args)
(thread-first
  (consult--async-sink)
  (consult-web--multi-dynamic-compute sources)
  (consult--async-throttle)
  (consult--async-split)))

(cl-defun consult-web--multi-dynamic (sources args &rest options)
  (let* ((sources (consult--multi-enabled-sources sources))
         (selected
          (apply #'consult--read
                 (consult-web--multi-dynamic-collection sources args)
                 (append
                  options
                  (list
                   :sort        t
                   :history     'consult-web--search-history
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

(defun consult-web--make-source-list (source-name request format annotate face narrow-char state preview-key category lookup group sort enabled selection-history)
  "Internal function to make a source for `consult--multi'.

Do not use this function directly, use `consult-web-define-source' macro
instead."
  `(:name ,source-name
          ,(if (and annotate face) :face)
          ,(if (and annotate face) (cond
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
                    (consult-web--lookup-function))
          :group ,(or group #'consult-web--group-function)
          :preview-key ,(and consult-web-show-preview (or preview-key consult-web-preview-key))
          ,(if enabled ':enabled)
          ,(if enabled enabled)
          :sort ,sort
          ))

(defun consult-web--call-static-command (input no-callback args request format face state source-name category lookup selection-history-var annotate preview-key sort)
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
(cl-defmacro consult-web-define-source (source-name &rest args &key type request format on-preview on-return state on-callback lookup dynamic group narrow-char category search-history selection-history face annotate preview-key docstring enabled sort &allow-other-keys)
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

REQUEST     (function)      Fetch results from source

FORMAT      (function)      Formats a single candidate

ON-PREVIEW  (function)      Preview action in `consult--read'

ON-RETURN   (function)      Return action in `consult--read'

STATE       (function)      STATE passed to `consult--read'
                            (bypasses ON-PREVIEW and ON-RETURN)

ON-CALLBACK (function)      Function called on selected candidate

DYNAMIC     (boolean/'both) Whether to make dynamic or non-dynamic commands

GROUP       (function)      Passed as GROUP to `consult--read'

ANNOTATE    (function)      Passed as ANNOTATE to `consult--read'

NARROW-CHAR (char)          Ppassed as NARROW to `consult-read'

CATEGORY    (symbol)        Passed as CATEGORY to `consult--read'

HISTORY     (symbol)        Passed as HISTORY to `consult--read'

FACE        (face)          Passed as FACE to `consult--read-multi'

PREVIEW-KEY (key)           Passed as PREVIEW-KEY to `consult--read'

DOCSTRING   (string)        DOCSTRING for the variable created for SOURCE-NAME

===================================================================

Detailed Decription:

REQUEST is a function that takes at least one string argument, INPUT, which is
the search term, and potentially other arguments. Keyword arguments
(e.g. by using `cl-defun') can be passed to this function from
minibuffer prompt using
`consult-async' commandline arguments.
Examples can be found in the wiki pages of the repo or in
“consult-web-sources.el” on the repository webpage or :
URL `https://github.com/armindarvish/consult-web/blob/main/consult-web-sources.el'


FORMAT takes a hashtable and returns a cons with a propertized string as key
 and plist property as value. For an example see
`consult-web--table-to-formatted-candidate-simple' or `consult-web--table-to-formatted-candidate-searchable'.

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
`consult--read' or `consult--multi'. See consult's Documentaion for more
 details.

FACE is passed to `consult-multi'. See consult's Documentaion for more
details.


DOCSTRING is used as docstring for the variable consult-web--source-%s
variable that this macro creates for %s=SOURCE-NAME.
"
  (if (symbolp source-name) (setq source-name (eval source-name)))

  `(progn

     ;; make a variable called consult-web--source-%s (%s=source-name)
     (defvar ,(consult-web--source-name source-name) (consult-web--make-source-list ,source-name ,request ,format ,annotate ,face ,narrow-char ,state ,preview-key ,category ,lookup ,group ,sort ,enabled ,selection-history))

     ;; make a function that creates a consult--read source for consult-web-multi
     (defun ,(consult-web--source-name source-name "-list") (input &rest args)
       ,(or docstring (consult-web--source-generate-docstring source-name))
       (consult-web--make-source-list ,source-name ,request ,format ,annotate ,face ,narrow-char ,state ,preview-key ,category ,lookup ,group ,sort ,enabled ,selection-history)
       )

     ;; make a static interactive command consult-web-%s (%s=source-name)
     (unless (eq ,dynamic t)
       (defun ,(consult-web--func-name source-name nil "-static") (&optional input no-callback &rest args)
         ,(or docstring (consult-web--func-generate-docstring source-name))
         (interactive "P")
         (consult-web--call-static-command input no-callback args ,request ,format ,face ,state ,source-name ,category ,lookup ,selection-history ,annotate ,preview-key ,sort)
         ))

     ;; make a dynamic interactive command consult-web-dynamic-%s (%s=source-name)
     (if ,dynamic
         (defun ,(consult-web--func-name source-name) (&optional initial no-callback &rest args)
           ,(or docstring (consult-web--func-generate-docstring source-name t))
           (interactive "P")
           (consult-web--call-dynamic-command initial no-callback args ,source-name ,request ,category ,face ,lookup ,search-history ,selection-history ,preview-key ,sort)
           ))

     ;; add source to consult-web-sources-alist
     (add-to-list 'consult-web-sources-alist (cons ,source-name
                                                          (list :name ,source-name
                                                                :source (consult-web--source-name ,source-name "-list")
                                                                :face ,face
                                                                :request-func ,request
                                                                :format-func (or ,format #'consult-web--table-to-formatted-candidate-searchable)


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
                                                                :type ,type
                                                                )))

     ,source-name))

;;;###autoload
(cl-defmacro consult-web--make-fetch-function (source &rest args &key source-name docstring &allow-other-keys)
  "Make a function for fetching result based on SOURCE.

SOURCE is a source for consult (e.g. a plist that is passed
to consult--read). See `consult-buffer-sources' for examples.
SOURCE-NAME is a string name for SOURCE
DOCSTRING is the docstring for the function that is returned."
  (let* ((source (if (plistp source) source (eval source)))
        (source-name (substring-no-properties (plist-get source :name))))
  `(progn
     ;; make a function that creates a consult--read source for consult-web-multi
     (defun ,(consult-web--source-name source-name "-fetch-results") (input &rest args)
       ,(or docstring (consult-web--source-generate-docstring source-name))
  (let ((results (funcall (plist-get ',source :items)))
        (source (substring-no-properties (plist-get ',source :name))))
    (cl-loop for a in results
             if (string-match (concat ".*" input ".*") a)
             collect
             (let* ((table (make-hash-table :test 'equal))
                    (title a))
           (puthash :title title
                    table)
           (puthash :url nil
                    table)
           (puthash :query input
                    table)
           (puthash :source (substring-no-properties source)
                    table)
           table)))))))

(cl-defun consult-web--make-source-from-consult-source (consult-source &rest args &key request format on-preview on-return state on-callback group narrow-char category dynamic search-history selection-history face annotate preview-key docstring &allow-other-keys)
"Makes a consult-web source from a consult source, CONSULT-SOURCE.
All othe input variables are passed to `consult-web-define-source'
macro. See `consult-web-define-source' for more details"
  (if (boundp consult-source)
        (let* ((source (eval consult-source))
               (source (if (plistp source) source (eval source)))
               (name (and (plistp source) (substring-no-properties (plist-get source :name))))
               (preview-key (or preview-key (and (plistp source) (plist-get source :preview-key))))
               (narrow-char (or narrow-char (and (plistp source) (plist-get source :narrow))))
               (narrow-char (if (listp narrow-char) (car narrow-char)))
               (face (if (member :face args) face (and (plistp source) (plist-get source :face))))
               (state (if (member :state args) state (and (plistp source) (plist-get source :state))))
               (annotate (if (member :annotate args) annotate (and (plistp source) (plist-get source :annotate))))
               (preview-key (or preview-key (and (plistp source) (plist-get source :preview-key)) consult-web-preview-key))
               (group (or group (and (plistp source)(plist-get source :group))))
               (category (or category (and (plistp source) (plist-get source :category)) 'consult-web)))
          (eval (macroexpand
           `(consult-web-define-source ,name
                                     :docstring ,docstring
                                     :annotate ',annotate
                                     :narrow-char ,narrow-char
                                     :category ',category
                                     :request (or ,request (consult-web--make-fetch-function ,source))
                                     :format ',format
                                     :face ',face
                                     :search-history ',search-history
                                     :selection-history ',selection-history
                                     :on-preview ',on-preview
                                     :on-return ',on-return
                                     :on-callback ',on-callback
                                     :preview-key ,preview-key
                                     :group ',group
                                     :dynamic ',dynamic))))
    (display-warning :warning (format "Consult-web: %s is not available. Make sure `consult-notes' is loaded and set up properly" consult-source)))
  )

;;; Frontend Interactive Commands
(defun consult-web-multi (&optional input sources no-callback &rest args)
  "Interactive “multi-source search”

INPUT is the initial search query.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-multi-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.
"
  (interactive "P")
  (let* ((input (or input
                    (and consult-web-default-autosuggest-command (funcall-interactively consult-web-default-autosuggest-command))
                    (consult-web--read-search-string)))
         (sources (or sources consult-web-multi-sources))
         (sources (remove nil (mapcar (lambda (source) (plist-get (cdr (assoc source consult-web-sources-alist)) :source)) sources)))
         (candidates (consult--slow-operation "The web is a big place, allow me a few seconds..." (mapcar (lambda (func) (funcall func input args)) sources)))
         (selected (consult--multi candidates
                                   :require-match nil
                                   :prompt (concat "[" (propertize "consult-web-multi" 'face 'consult-web-prompt-face) "]" " Search:  ")
                                   :sort t
                                   :annotate nil
                                   :category 'consult-web
                                   :history 'consult-web--selection-history
                                   ))
         (source (get-text-property 0 :source (car selected)))
         )
    (unless no-callback
      (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) (car selected)))
    (car selected)
    ))

(defun consult-web-dynamic (&optional initial sources no-callback &rest args)
  "Interactive “multi-source dynamic search”

INITIAL is the initial search prompt in minibuffer.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-dynamic-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.

This is an interactive command that fetches results form all the sources in `consult-web-dynamic-sources' with dynamic completion meaning that the search term can be dynamically updated by the user
and the results are fetched as the user types.

Additional commandline arguments can be passed in the minibuffer
entry similar to `consult-grep' by typing `--` followed by arguments.

For example the user can enter:

`#consult-web -- -g domain'

this will run a search on all the `consult-web-dynamic-sources' for
the term “consult-web” and then groups the results by the “domain
of the URL” of the results.

Built-in arguments include:

 -g, --groups, or :groups  for grouping (see `consult-web-group-by' and `consult-web--override-group-by'. for more info)

 -n, --count, or :count is passed as the value for COUNT to any source in `consult-web-dynamic-sources'.
If the request function for the source takes a keyword argument for COUNT (e.g. :count value), this is used as the value otherwise it is ignored.

 -p, --page, or :page is passed as the value for PAGE to any source in `consult-web-dynamic-sources'.
If the request function for the source takes a keyword argument for page (e.g. :page value), this is used as the value otherwise it is ignored.

Custom arguments can be passed by using “--ARG value” (or “:ARG value”).
For example, if the user types the following in the minibuffer:
“#how to do web search in emacs? -- --model gpt-4”
The term “how to do web search in emacs?” is passed as the search
term and the “gpt-4” as a keyword argument for :model to every
source in `consult-web-dynamic-sources'. If any request function of
the sources takes a keyword argument for :model, “gpt-4” is
used then.

Once the results are fetched, narrowing down can be done by using “#” after the serach term similar to `consult-grep'.
For example:
“#consult-web#github.com”
uses “consult-web” as the search term, and then narrows the choices to
results that have “github.com” in them.

For more examples, refer to the official documentation of the repo here:
URL `https://github.com/armindarvish/consult-web'.

For more details on consult--async functionalities, see `consult-grep'
and the official manual of consult, here: URL `https://github.com/minad/consult'."
  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-dynamic-sources))
         (request-sources (remove nil (mapcar (lambda (source)
(plist-get (cdr (assoc source consult-web-sources-alist)) :request-func)) sources)))
         (prompt (concat "[" (propertize "consult-web-dynamic" 'face 'consult-web-prompt-face) "]" " Search:  "))
         (collection (consult-web-dynamic--collection request-sources nil nil args))
         (selected (consult-web-dynamic--internal prompt collection initial 'consult-web nil 'consult-web--search-history))
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

This is similar to `consult-web-dynamic', but runs the search on academic literature sources in `consult-web-scholar-sources'.
Refer to `consult-web-dynamic' for more details."
  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-scholar-sources))
         (request-sources (remove nil (mapcar (lambda (source)
                                                (plist-get (cdr (assoc source consult-web-sources-alist)) :request-func)) sources)))
         (collection (consult-web-dynamic--collection request-sources nil nil args))
         (selected (consult-web-dynamic--internal (concat "[" (propertize "consult-web-scholar" 'face 'consult-web-prompt-face) "]" " Search:  ") collection initial 'consult-web-scholar nil 'consult-web--search-history))
         (source (get-text-property 0 :source selected)))
    (unless no-callback
      (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected)
      )
    selected
    ))

(defun consult-web-omni-get-sources (&optional input)
"Returns a flat list of candidates for input.

Passes input to sources in `consult-web-omni-sources' and returns a
flattend list of sources."
(apply #'append (mapcar (lambda (item) (cond
                                        ((stringp item)
                                         (if-let ((func (plist-get (cdr (assoc item consult-web-sources-alist)) :source)))
                                             (list (funcall func input))))
                                        ((symbolp item)
                                         (eval item))))

 consult-web-omni-sources)))

(defun consult-web-omni (&optional input sources no-callback &rest args)
"Interactive “multi-source omni” search.
This is for using combination of web and local sources defined in
`consult-web-omni-sources'.

Passes INPUT to SOURCES and returns results in minibuffer.
If SOURCES is nil, `consult-web-omni-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action."
  (interactive)
  (let* ((input (or input  (consult-web-brave-autosuggest input) ""))
         (consult-web-default-count 10)
         (sources (or sources (consult-web-omni-get-sources input)))
         (selected (consult--multi sources
                                   :prompt "Select: "
                                   :history 'consult-web--omni-history
                                   :add-history (list (thing-at-point 'word t)
                                                      "")
                                   :sort t
                                   :initial input
                                   ))
         (source (get-text-property 0 :source (car selected))))
    (unless no-callback
      (cond
       ((and source (member source (mapcar #'car consult-web-sources-alist)))
        (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) (car selected)))
       ((and (bufferp (car selected)) (buffer-live-p (car selected)))
        (consult--buffer-action (car selected)))
       (t nil))
      )
    (car selected)
    ))

(defun consult-web-dynamic-omni (&optional initial sources no-callback &rest args)
  "Interactive “multi-source and dynamic omni search”
This is for using combination of web and local sources defined in
`consult-web-dynamic-omni-sources'.

INITIAL is the initial search prompt in minibuffer.
Searches all sources in SOURCES. if SOURCES is nil
`consult-web-dynamic-omni-sources' is used.
If NO-CALLBACK is t, only the selected candidate is returned without
any callback action.

This is a dynamic command and additional arguments can be passed in
the minibuffer. See `consult-web-dynamic' for more details."

  (interactive "P")
  (let* ((consult-async-refresh-delay consult-web-dynamic-refresh-delay)
         (consult-async-input-throttle consult-web-dynamic-input-throttle)
         (consult-async-input-debounce consult-web-dynamic-input-debounce)
         (sources (or sources consult-web-dynamic-omni-sources))
         (request-sources (remove nil (mapcar (lambda (source)
                                                (plist-get (cdr (assoc source consult-web-sources-alist)) :request-func)) sources)))
         (prompt (concat "[" (propertize "consult-web-dynamic-omni" 'face 'consult-web-prompt-face) "]" " Search:  "))
         (collection (consult-web-dynamic--collection request-sources nil nil args))
         (selected (consult-web-dynamic--internal prompt collection initial 'consult-web nil 'consult-web--search-history))
         (source (get-text-property 0 :source selected)))
    (unless no-callback
      (funcall (plist-get (cdr (assoc source consult-web-sources-alist)) :on-callback) selected))
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
