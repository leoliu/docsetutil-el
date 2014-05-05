;;; docsetutil.el --- use Cocoa/iOS documentations in emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.7
;; Keywords: c, processes, tools, docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; References:
;;;  - Use xref system from the help facility - check out help-mode.el

;;; Code:

(require 'cl-lib)
(require 'url-parse)

(eval-and-compile
  (unless (fboundp 'user-error)
    (defalias 'user-error 'error)))

(defgroup docsetutil nil
  "Group for docsetutil."
  :prefix "docsetutil-"
  :group 'tools)

(defcustom docsetutil-program
  (or (and (executable-find "docsetutil") "docsetutil")
      (and (file-exists-p "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
           "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
      "docsetutil")
  "Executable for docsetutil.
Normally it is in the following directory:
  /Applications/Xcode.app/Contents/Developer/usr/bin/"
  :type 'file
  :group 'docsetutil)

(defcustom docsetutil-cache-directory (locate-user-emacs-file "cache/docsets")
  "Directory for docset caches.
Set to nil to disable caching to disk."
  :type 'directory
  :group 'docsetutil)

(defcustom docsetutil-fill-column 75
  "Fill column used for formatting docset search results."
  :type 'integer
  :group 'docsetutil)

(defcustom docsetutil-fontify-declaration t
  "Fontify declarations using cc mode."
  :type 'boolean
  :group 'docsetutil)

(defcustom docsetutil-use-text-tree t
  "Use hierarchical/tree format to display full text search results.
When set, individual page and section search results are
coallesced together under the node that holds those search
results."
  :type 'boolean
  :group 'docsetutil)

(defcustom docsetutil-browse-url-function #'browse-url
  "Function used to browse URL in search outputs."
  :type 'function
  :group 'docsetutil)

(defcustom docsetutil-current-word-function 'current-word
  "Function used by `docsetutil-search' to get a default argument."
  :type 'function
  :group 'docsetutil)

;;;; END OF DEFCUSTOMS

;; See: http://goo.gl/jlQX0w
(defvar docsetutil-docset-search-paths
  '("/Applications/Xcode.app/Contents/Developer/Documentation/DocSets"
    "/Developer/Documentation/DocSets"
    "~/Library/Developer/Shared/Documentation/DocSets"
    "/Library/Developer/Shared/Documentation/DocSets"
    "/Network/Library/Developer/Shared/Documentation/DocSets"
    "/System/Library/Developer/Shared/Documentation/DocSets")

  "A list of directories where XCode search for docsets.")

(defvar docsetutil-docset-path nil
  "The docset to use by `docsetutil-search'.")

(defvar docsetutil-search-history nil)

(defconst docsetutil-api-regexp "^ \\(.*?\\)   \\(.*?\\) -- \\(.*\\)$")

(defun docsetutil-trim (s)
  "Strip leading and trailing blank chars in string S."
  (replace-regexp-in-string "^[ \t\n]*\\(.*?\\)[ \t\n]*$" "\\1" s))

(defun docsetutil-run (buffer &rest args)
  (apply #'process-file docsetutil-program nil buffer nil
         (cl-remove-if #'null args)))

;;; Completion

(defvar docsetutil-cache nil)           ; (CACHE-ID . HASH-TABLE)

(defun docsetutil-cache-id (docset)
  (let ((info (docsetutil-parse-docset-info docset t)))
    (concat (cdr (assoc "CFBundleIdentifier" info))
            "-v"
            (cdr (assoc "CFBundleVersion" info)))))

(defun docsetutil-cache-read (cache-id)
  (when docsetutil-cache-directory
    (let ((file (expand-file-name cache-id docsetutil-cache-directory)))
      (when (and (file-exists-p file)
                 (/= 0 (nth 7 (file-attributes file))))
        (let ((cache (with-temp-buffer
                       (insert-file-contents file)
                       (condition-case nil
                           (read (current-buffer))
                         (error
                          (error "Failed to load malformed cache file: %s" file)))))
              (ob (make-vector 37 0)))
          (dolist (item cache)
            (intern item ob))
          (setq docsetutil-cache (cons cache-id ob))
          ob)))))

(defun docsetutil-cache-write (cache-id coll)
  (when docsetutil-cache-directory
    (or (file-exists-p docsetutil-cache-directory)
        (make-directory docsetutil-cache-directory t))
    (let ((file (expand-file-name cache-id docsetutil-cache-directory)))
      (with-temp-buffer
        (prin1 (cl-loop for s being the symbols of coll collect (symbol-name s))
               (current-buffer))
        (write-region nil nil file nil 0)))))

(defun docsetutil-completions (query &optional path)
  "Return a collection of names in the output of QUERY to a docset.
Mutiple queries can be specified by seperating them with space.
PATH is the path to the docset and defaults to
`docsetutil-docset-path'."
  (cl-check-type query string)
  (or path
      docsetutil-docset-path
      (error "No docset path provided"))
  (let ((path (or path docsetutil-docset-path)))
    (with-temp-buffer
      (cl-assert (zerop (call-process docsetutil-program nil t nil
                                      "search" "-skip-text" "-query" query
                                      path))
                 nil "Process %s failed with non-zero exit code:\n%s"
                 docsetutil-program (buffer-string))
      (goto-char (point-min))
      (let (collection)
        (while (re-search-forward
                "^[ \t]*[^/]+/[^/]+/[^/]+/\\([^ \t\r\n]+\\)" nil t)
          (or collection (setq collection (make-vector 17 0)))
          (intern (match-string 1) collection))
        collection))))

;; Benchmark on an iMac 2.7 GHz Intel Core i5
;;  - iOS 6.0 Library: 2.76 seconds (0.18 seconds from disk cache)
;;  - OS X 10.8 Core Library: 8.56 seconds (0.9 seconds from disk cache)
(defun docsetutil-objc-completions (&optional docset)
  "Return completions for C and Objective-C."
  (let* ((docset (or docset docsetutil-docset-path))
         (cache-id (docsetutil-cache-id docset))
         (cache (if (equal (car docsetutil-cache) cache-id)
                    (cdr docsetutil-cache)
                  (docsetutil-cache-read cache-id))))
    (or cache
        (let ((coll (progn
                      (message "Prepare docset completions...")
                      (docsetutil-completions
                       "C/*/*/* C++/*/*/* Objective-C/*/*/*" docset))))
          ;; Cache it unless empty.
          (when coll
            (setq docsetutil-cache (cons cache-id coll))
            (docsetutil-cache-write cache-id coll))
          coll))))

;;;###autoload
(defun try-docsetutil-objc-completions (old)
  "A function suitable for `hippie-expand-try-functions-list'."
  (eval-and-compile (require 'hippie-exp))
  (unless old
    (he-init-string (save-excursion
                      (skip-syntax-backward "w_")
                      (point))
                    (point))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (sort (all-completions he-search-string
                                      (docsetutil-objc-completions))
                     #'string-lessp))))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        nil)
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

;;; UI for selecting a docset

(defun docsetutil-insert-plist-contents (file)
  (save-restriction
    (narrow-to-region (point) (point))
    (insert-file-contents file)
    (when (looking-at-p "^bplist")
      ;; /usr/libexec/PlistBuddy or /usr/bin/plutil can convert bplist
      ;; to xml1 format.
      (or (executable-find "plutil")
          (error "Can not process binary plist file"))
      (cl-assert (zerop (shell-command-on-region
                         (point-min) (point-max)
                         "plutil -convert xml1 -o - -" nil t))
                 nil "Convert `%s' to xml failed" file))))

(defun docsetutil-normalise-plist-keyvals (elements)
  (cl-loop for x in elements
           when (consp x)
           collect (pcase (car x)
                     (`array (docsetutil-normalise-plist-keyvals (cddr x)))
                     (_ (cl-third x)))))

(defun docsetutil-parse-plist-region (beg end)
  "Parse a plist region and return an alist for the key-value pairs."
  (let ((keyvals (docsetutil-normalise-plist-keyvals
                  (if (fboundp 'libxml-parse-xml-region)
                      (cddr (cl-caddr (libxml-parse-xml-region beg end)))
                    (eval-and-compile (require 'xml))
                    (xml-node-children
                     (car (xml-get-children
                           (car (xml-parse-region beg end)) 'dict)))))))
    (cl-loop for (k v) on keyvals by #'cddr
             collect (cons k v))))

(defun docsetutil-parse-docset-info (docset &optional error)
  "Parse Info.plist file of a docset if present.
DOCSET is the path to a docset and defaults to
`docsetutil-docset-path'. If ERROR is non-nil, signal an error
when Info.plist is missing.

The return value is an alist of (KEY . VALUE) both KEY and VALUE
are strings."
  (let* ((docset (or docset docsetutil-docset-path))
         (infofile (expand-file-name "Contents/Info.plist" docset)))
    (cond
     ((and error (not (file-exists-p infofile)))
      (error "No such file `%s'" infofile))
     ((file-exists-p infofile)
      (with-temp-buffer
        (docsetutil-insert-plist-contents infofile)
        (docsetutil-parse-plist-region (point-min) (point-max)))))))

(defun docsetutil-find-all-docsets (&optional paths)
  "Find all docsets in PATHS.
Each item in the return value has the form:
  (Fullpath CFBundleIdentifier CFBundleName Info)."
  (apply 'append
         (mapcar (lambda (path)
                   (mapcar (lambda (d)
                             (let* ((info (docsetutil-parse-docset-info d))
                                    (id (cdr (assoc "CFBundleIdentifier" info)))
                                    (name (cdr (assoc "CFBundleName" info))))
                               (cl-list* d id name info)))
                           (directory-files path t "\\.docset\\'")))
                 (cl-loop for p in (or paths docsetutil-docset-search-paths)
                          when (file-directory-p p) collect p))))

(defun docsetutil-view-docset-info (docset &optional all)
  "View DOCSET information.
When called interactively with no prefix, view current docset;
with two prefixes, view all docsets; otherwise ask the user for a
docset to view."
  (interactive
   (list (if (or (not docsetutil-docset-path)
                 (and current-prefix-arg
                      (/= (prefix-numeric-value current-prefix-arg) 16)))
             (completing-read "Docset: " (docsetutil-find-all-docsets))
           docsetutil-docset-path)
         (= (prefix-numeric-value current-prefix-arg) 16)))
  (or docset (error "No docset provided"))
  (let ((docset (if (stringp docset)
                    (assoc docset (docsetutil-find-all-docsets))
                  docset))
        (fmt-docset
         (lambda (doc)
           (with-current-buffer standard-output
             (insert (propertize (format "[From %s]"
                                         (file-name-directory (car doc)))
                                 'face 'font-lock-comment-face) "\n\n")
             (insert (propertize (file-name-nondirectory (car doc))
                                 'face 'bold-italic) ":\n\n")
             (when (cl-cdddr doc)
               (cl-loop for (k . v) in (cl-cdddr doc)
                        with fmt = (format "%%%ds: "
                                           (apply 'max
                                                  (mapcar (lambda (x)
                                                            (length (car x)))
                                                          (cl-cdddr doc))))
                        do
                        (insert (propertize (format fmt k) 'face 'bold))
                        (insert (cond
                                 ((consp v) (mapconcat 'identity v " "))
                                 ((stringp v) v)
                                 (t "(none)")) "\n")))
             (insert (make-string 75 ?-) "\n\n")))))
    (with-output-to-temp-buffer "*DocsetInfo*"
      (if all (mapc fmt-docset (docsetutil-find-all-docsets))
        (funcall fmt-docset docset)))))

(defun docsetutil-read-docset ()
  (save-window-excursion
    (let ((docsets (docsetutil-find-all-docsets))
          (split-width-threshold nil)
          (buf " *docsets*")
          (i 0)
          number
          default)
      (with-output-to-temp-buffer buf
        (mapc (lambda (docset)
                (cl-incf i)
                (pcase-let ((`(,path ,_ ,bn . ,info) docset))
                  (let ((ver (cdr (assoc "CFBundleVersion" info))))
                    (princ (format "%-2d => %s%s" i
                                   (if ver (format "(v%s) " ver) "")
                                   (or bn (file-name-nondirectory path))))
                    (when (equal docsetutil-docset-path path)
                      (setq default i)
                      (princ " (current)"))
                    (princ "\n"))))
              docsets))
      (with-current-buffer buf
        (setq truncate-lines t)
        (when default
          (forward-line (1- default)))
        (let ((inhibit-read-only t))
          (put-text-property (line-beginning-position) (line-end-position)
                             'face 'bold-italic)))
      (fit-window-to-buffer (get-buffer-window buf))
      (setq number (read-number "Choose a docset: " default))
      (car (nth (1- number) docsets)))))

;;;###autoload
(defun docsetutil-choose-docset (docset)
  "Choose a DOCSET from the list by `docsetutil-find-all-docsets'."
  (interactive (list (docsetutil-read-docset)))
  (if (not docset)
      (message "No docset specified")
    (setq docsetutil-docset-path docset)
    (when (called-interactively-p 'interactive)
      (message "Docset: %s" docset))
    docset))

;;; Docset Query

(eval-when-compile (require 'html2text)) ; for html2text-replace-list

(defun docsetutil-wash-html-tags (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let (keyword href)
      (while (re-search-forward "<\\([^> ]+\\)[ \t\n]*\\(.*?\\)>\\(\\(?:.\\|\n\\)*?\\)</\\1>" nil t)
        (setq keyword (match-string 3))
        (if (equal (downcase (match-string 1)) "a")
            (progn
              ;; If there are spaces inside KEYWORD, treat it as
              ;; external link.
              (when (string-match-p "[ \t\n]+" keyword)
                (setq href (match-string 2)))
              (replace-match "")
              (when (and href (string-match "href=\"\\([^\"]+\\)\"" href))
                (setq href (concat "file://"
                                   (expand-file-name "Contents/Resources/Documents/"
                                                     docsetutil-docset-path)
                                   (match-string 1 href))))
              (insert-text-button keyword
                                  'help-function (if href
                                                     docsetutil-browse-url-function
                                                   #'docsetutil-search)
                                  'help-args (list (or href keyword))
                                  'face (if href 'link 'font-lock-keyword-face)
                                  :type 'help-xref))
          (replace-match keyword nil t))
        (goto-char (match-beginning 0))))
    (goto-char (point-min))
    (let* ((html-replace-list (eval-when-compile html2text-replace-list))
           (re (regexp-opt (mapcar 'car html-replace-list))))
      (while (re-search-forward re nil t)
        (replace-match
         (cdr (assoc (match-string 0) html-replace-list)))))))

(defun docsetutil-setup-cc-buffer (&optional buf)
  (let ((buf (or buf " *docsetutil cc mode*")))
    (or (get-buffer buf)
        (with-current-buffer (get-buffer-create buf)
          (objc-mode)
          (setq font-lock-mode t)
          (funcall font-lock-function font-lock-mode)
          (add-hook 'change-major-mode-hook 'font-lock-change-mode nil t)
          (current-buffer)))))

(defun docsetutil-fontify-cc-string (string)
  (with-current-buffer (docsetutil-setup-cc-buffer)
    (erase-buffer)
    (insert string)
    (font-lock-default-fontify-region (point-min) (point-max) nil)
    (buffer-string)))

(defun docsetutil-format-api-row (name beg name-end end)
  (put-text-property beg name-end 'face 'bold)
  (let ((fp fill-prefix)
        (fc fill-column)
        (end (copy-marker end)))
    (setq fill-column docsetutil-fill-column)
    (unwind-protect
        (cond
         ((member name '("Abstract:" "Return Value:" "Availability:"))
          (setq fill-prefix (make-string (- (current-column) 6) ?\s))
          (fill-region-as-paragraph name-end end))
         ((and docsetutil-fontify-declaration (equal name "Declaration:"))
          (insert (docsetutil-fontify-cc-string
                   (delete-and-extract-region name-end end))))
         ((equal name "Parameters:")
          (setq fill-prefix (make-string (+ 4 (current-column)) ?\s))
          ;; Test: arrayWithObjects: and addObserver:selector:name:object:
          (while (re-search-forward "\\([^\r\n]+?\\)[ \t]\\{2,\\}\\(.*\\)" end t)
            (put-text-property (match-beginning 1) (match-end 1)
                               'face '(:underline t :inherit italic))
            (goto-char (match-beginning 2))
            (fill-region-as-paragraph (point) (match-end 2))
            (skip-chars-forward " \t\r\n")
            (when (> (point) end)
              (goto-char end)))))
      (setq fill-prefix fp
            fill-column fc)
      (set-marker end nil))))

(defun docsetutil-highlight-search-results (&optional buffer)
  "Highlight docsetutil search results in BUFFER.
The default value for BUFFER is current buffer."
  (let ((inhibit-read-only t) path missing help-function help-args)
    (with-current-buffer (or buffer (current-buffer))
      (save-excursion
        (docsetutil-wash-html-tags)
        (goto-char (point-min))
        ;; process API results
        (while (re-search-forward docsetutil-api-regexp nil t)
          (if (file-name-absolute-p (match-string 2))
              (setq path (match-string-no-properties 2))
            (setq path (substring-no-properties
                        (concat (match-string 3) (match-string 2)))))
          (save-match-data
            (cond
             ;; xcode 3.x: /usr/share/man/man2/open.2.gz
             ;; xcode 4.x: documentation/Darwin/Reference/ManPages/man2/open.2.html#//apple_ref/c/func/open
             ((string-match "/\\(?:man\\|ManPages\\)/man\\([1-9]\\)/\\(.*\\)\\.\\1\\." path)
              (setq help-function 'man
                    help-args (list (concat (match-string 1 path) " "
                                            (match-string 2 path)))))
             ((string-match "\\.html" path)
              (setq help-function docsetutil-browse-url-function
                    help-args (list (if (url-type (url-generic-parse-url path))
                                        path
                                      (concat "file://" path)))))))
          (delete-region (match-end 1) (line-end-position))
          (make-text-button
           (match-beginning 1) (match-end 1)
           'face 'link
           'help-echo path
           'help-function help-function
           'help-args help-args
           :type 'help-xref)
          (let ((limit (save-excursion
                         (and (re-search-forward "^$" nil t)
                              (point-marker))))
                (field-re "^[ \t]*\\([[:upper:]][[:word:][:blank:]]+:\\) "))
            (while (re-search-forward field-re limit t)
              (let ((name (match-string 1))
                    (name-end (point))
                    (beg (match-beginning 1))
                    (end (min (or limit (point-max))
                              (if (re-search-forward field-re limit t)
                                  (match-beginning 0)
                                limit))))
                (goto-char name-end)
                (docsetutil-format-api-row name beg name-end end)
                (when (and limit (> (point) limit))
                  (goto-char limit))))
            (set-marker limit nil)))
        ;; process full text results
        (while (re-search-forward "^[ \t]+[0-9.]+ \\(.*\\)$" nil t)
          (setq path (match-string-no-properties 1))
          (setq help-function docsetutil-browse-url-function)
          (if (save-match-data (url-type (url-generic-parse-url path)))
              (setq help-args (list path))
            (setq path (concat (expand-file-name "Contents/Resources/Documents/"
                                                 docsetutil-docset-path)
                               path))
            (when (setq missing (not (file-exists-p path)))
              (save-match-data
                (and (string-match "\\(.*\\)#" path)
                     (setq missing (not (file-exists-p (match-string 1 path)))))))
            (setq help-args (list (concat "file://" path))))
          (if missing
              (put-text-property (match-beginning 1) (match-end 1)
                                 'face 'shadow)
            (make-text-button
             (match-beginning 1) (match-end 1)
             'face (if missing 'shadow 'link)
             'help-function help-function
             'help-args help-args
             :type 'help-xref)))))))

(defun docsetutil-completing-read (&optional fulltext)
  (or docsetutil-docset-path
      (call-interactively #'docsetutil-choose-docset))
  (or (and docsetutil-docset-path
           (file-exists-p docsetutil-docset-path))
      (error "DocSet `%s' does not exist" docsetutil-docset-path))
  (let ((default (funcall (or docsetutil-current-word-function
                              #'current-word))))
    (docsetutil-trim
     (completing-read
      (format (if default "Apple docset %s search (default: %s): "
                "Apple docset %s search: ")
              (if fulltext "full text" "API") default)
      (docsetutil-objc-completions)
      nil nil nil 'docsetutil-search-history default))))

(define-obsolete-function-alias 'docsetutil-search 'docsetutil-api "2014-05-05")

;;;###autoload
(defun docsetutil-api (term &optional raw browse)
  "Use `docsetutil' to search documentation on TERM.
With prefix, also include full text search results."
  (interactive (list (docsetutil-completing-read) current-prefix-arg))
  ;; For API search, space is used to separate terms i.e. 'nsstring
  ;; nsnumber' returns results for both NSString and NSNumber.
  (with-temp-buffer
    (docsetutil-run t "search" "-skip-text" "-verbose"
                    "-query" term docsetutil-docset-path)
    (goto-char (point-min))
    (cond
     ((and (not (string-match-p "[ \t]+" term)) ;Single term search
           (re-search-forward "\\`Found total of 0 API matches in.*$" nil t))
      (user-error "%s" (match-string 0)))
     (browse
      (docsetutil-highlight-search-results)
      (car (button-get (forward-button 1) 'help-args)))
     (t
      (help-setup-xref (list #'docsetutil-api term raw)
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (princ (buffer-string))
        (unless raw
          (docsetutil-highlight-search-results standard-output)))
      (pcase (get-buffer-window (help-buffer))
        ((and w (guard w)) (fit-window-to-buffer w (floor (frame-height) 2))))))))

;;;###autoload
(defun docsetutil-browse-api (term)
  (interactive (list (docsetutil-completing-read)))
  (funcall docsetutil-browse-url-function (docsetutil-api term nil t)))

;;;###autoload
(defun docsetutil-fulltext (term &optional raw)
  (interactive (list (docsetutil-completing-read t) current-prefix-arg))
  (help-setup-xref (list #'docsetutil-fulltext term raw)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ "Full text search results:\n")
    (docsetutil-run standard-output
                    "search" (and docsetutil-use-text-tree "-text-tree")
                    "-query" term docsetutil-docset-path)
    (unless raw
      (docsetutil-highlight-search-results standard-output)))
  (pcase (get-buffer-window (help-buffer))
    ((and w (guard w)) (fit-window-to-buffer w (floor (frame-height) 2)))))

(provide 'docsetutil)
;;; docsetutil.el ends here
