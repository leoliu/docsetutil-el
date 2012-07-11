;;; docsetutil.el --- Emacs Interface to `docsetutil'

;; Copyright (C) 2011, 2012  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
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

(eval-when-compile (require 'cl))
(require 'url-parse)

(defgroup docsetutil nil
  "Group for docsetutil."
  :prefix "docsetutil-"
  :group 'tools)

(defcustom docsetutil-program "docsetutil"
  "Executable for docsetutil.

Normally it resides in one of the following directories:
  
  1. /Applications/Xcode.app/Contents/Developer/usr/bin/
     (xcode 4.2 and above)
  2. /Developer/usr/bin/ (xcode 3.x)"
  :type 'file
  :group 'docsetutil)

(defcustom docsetutil-fill-column 75
  "Fill column used for formatting docset search results."
  :type 'integer
  :group 'docsetutil)

(defcustom docsetutil-use-text-tree t
  "Use hierarchical/tree format to display full text search results.
When set, individual page and section search results are
coallesced together under the node that holds those search
results."
  :type 'boolean
  :group 'docsetutil)

(defcustom docsetutil-browse-url-function 'browse-url
  "Function used to browse url in search outputs."
  :type 'function
  :group 'docsetutil)

;;;; END OF DEFCUSTOMS

;; See: http://goo.gl/jiYPv
(defvar docsetutil-docset-search-paths
  '("/Applications/Xcode.app/Contents/Developer/Documentation/DocSets"
    "/Developer/Documentation/DocSets"
    "~/Library/Developer/Shared/Documentation/DocSets"
    "/Library/Developer/Shared/Documentation/DocSets"
    "/Network/Library/Developer/Shared/Documentation/DocSets"
    "/System/Library/Developer/Shared/Documentation/DocSets")

  "A list of directories where XCode search for docsets.")

(defvar docsetutil-docset-path (car (last (docsetutil-all-docsets)))
  "The docset to use by `docsetutil-search'.")

(defvar docsetutil-objc-completions nil)

(defvar docsetutil-search-history nil)

(defconst docsetutil-api-regexp "^ \\(.*?\\)   \\(.*?\\) -- \\(.*\\)$")

(defun docsetutil-all-docsets ()
  "Return all docsets in `docsetutil-docset-search-paths'."
  (loop for p in docsetutil-docset-search-paths
        when (file-directory-p p)
        append
        ;; Match non "." ".." names
        (loop for dir in (directory-files p t "^\\(?:[^.]\\|\\.[^.]\\)")
              when (file-directory-p dir)
              collect dir)))

;;; Completion

(defun docsetutil-completions (query &optional path)
  "Return a collection of names in the output of QUERY to a docset.
PATH is the path to the docset and defaults to
`docsetutil-docset-path'."
  (assert (or path docsetutil-docset-path) nil "No docset path specfied.")
  (let ((path (or path docsetutil-docset-path))
        (res (make-vector 17 0)))
    (with-temp-buffer
      (assert (zerop (call-process docsetutil-program nil t nil
                                   "search" "-skip-text" "-query" query
                                   path))
              nil "Process %s failed with non-zero exit code"
              docsetutil-program)
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*[^/]+/[^/]+/[^/]+/\\([^ \t\r\n]+\\)" nil t)
        (intern (match-string 1) res)))
    res))

;; Benchmark on an iMac 2.7 GHz Intel Core i5
;;  - iOS 5.1 Libarary: 2.56 seconds
;;  - OS X 10.7 Core Library: 7.35 seconds (5.3 seconds for C/*/*/*)
(defun docsetutil-objc-completions ()
  "Return completions for C and Objective-C."
  (or docsetutil-objc-completions
      (progn
        (message "Prepare docset completions...")
        (setq docsetutil-objc-completions
              (vconcat (docsetutil-completions "C/*/*/*")
                       (docsetutil-completions "Objective-C/*/*/*"))))))

(eval-when-compile (require 'hippie-exp))

;;;###autoload
(defun try-docsetutil-objc-completions (old)
  "A function suitable for `hippie-expand-try-functions-list'."
  (require 'hippie-exp)
  (unless old
    (he-init-string (save-excursion
                      (skip-syntax-backward "w_")
                      (point))
                    (point))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (sort (all-completions he-search-string
                                      (docsetutil-objc-completions))
                     'string-lessp))))
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
    (when (looking-at-p "^bplist00")
      ;; /usr/libexec/PlistBuddy or /usr/bin/plutil can convert bplist
      ;; to xml1 format.
      (or (executable-find "plutil")
          (error "Can not process binary plist file"))
      (assert (zerop (shell-command-on-region
                      (point-min) (point-max)
                      "plutil -convert xml1 -o - -" nil t))
              nil "Convert `%s' to xml failed" file))))

(eval-when-compile (require 'xml))      ; pacify compiler warnings
(defun docsetutil-parse-plist-region (beg end)
  "Parse a plist region and return an alist for the key-value pairs."
  (let ((children
         (if (fboundp 'libxml-parse-xml-region)
             (cddr (caddr (libxml-parse-xml-region beg end)))
           (require 'xml)
           (let ((rm-if (lambda (pred seq)
                          (delq nil (mapcar (lambda (x)
                                              (and (not (funcall pred x)) x))
                                            seq)))))
             (funcall rm-if 'stringp
                      (xml-node-children
                       (car (funcall rm-if 'stringp
                                     (xml-node-children
                                      (car (xml-parse-region beg end)))))))))))
    (loop for (x y) on children by 'cddr
          collect (cons (third x) (third y)))))

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
      (signal 'file-error (list 'file-exists-p infofile)))
     ((file-exists-p infofile)
      (with-temp-buffer
        (docsetutil-insert-plist-contents infofile)
        (docsetutil-parse-plist-region (point-min) (point-max)))))))

;;;###autoload
(defun docsetutil-choose-docset (docset)
  "Choose a DOCSET from all found by `docsetutil-all-docsets'."
  (interactive
   (save-window-excursion
     (let ((docsets (docsetutil-all-docsets))
           (split-width-threshold nil)
           (buf " *docsets*")
           number default)
       (with-output-to-temp-buffer buf
         (loop for docset in docsets
               for i from 1
               for name = (or (cdr (assoc "CFBundleName"
                                          (docsetutil-parse-docset-info docset)))
                              (file-name-nondirectory docset))
               do
               (princ (format "%-2d => %s" i name))
               (when (equal docsetutil-docset-path docset)
                 (setq default i)
                 (princ " (current)"))
               (princ "\n")))
       (with-current-buffer buf
         (setq truncate-lines t)
         (when default
           (forward-line (1- default)))
         (let ((inhibit-read-only t))
           (put-text-property (line-beginning-position) (line-end-position)
                              'face 'bold-italic)))
       (fit-window-to-buffer (get-buffer-window buf))
       (setq number (read-number "Choose a docset: " default))
       (list (nth (1- number) docsets)))))
  (when docset
    (setq docsetutil-docset-path docset
          docsetutil-objc-completions nil)
    (when (called-interactively-p 'interactive)
      (message "Docset: %s" docset))))

;;; Docset Query

(defun docsetutil-wash-html-tags (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let (keyword href)
      (while (re-search-forward "<\\([^> ]+\\)[ \t\n]*\\(.*?\\)>\\(\\(?:.\\|\n\\)*?\\)</\\1>" nil t)
        (setq keyword (match-string 3))
        (if (equal (match-string 1) "a")
            (progn
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
                                  'face 'link
                                  :type 'help-xref))
          (replace-match keyword nil t))
        (goto-char (match-beginning 0))))))

(defun docsetutil-highlight-search-results (&optional buffer)
  "Highlight docsetutil search results in BUFFER.
The default value for BUFFER is current buffer."
  (let ((inhibit-read-only t) path missing help-function help-args)
    (with-current-buffer buffer
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
             ((string-match "\\.html" path)
              (setq help-function docsetutil-browse-url-function
                    help-args (list (if (url-type (url-generic-parse-url path))
                                        path
                                      (concat "file://" path)))))
             ;; xcode 3.x: /usr/share/man/man2/open.2.gz
             ;; xcode 4.x: documentation/Darwin/Reference/ManPages/man2/open.2.html#//apple_ref/c/func/open
             ((string-match "/man/man\\([1-9]\\)/\\(.*\\)\\.[1-9]\\." path)
              (setq help-function 'man
                    help-args (list (concat (match-string 1 path) " "
                                            (match-string 2 path)))))))
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
              (put-text-property (match-beginning 1) (match-end 1) 'face 'bold)
              (when (equal (match-string 1) "Abstract:")
                (let ((fp fill-prefix)
                      (fc fill-column))
                  (setq fill-prefix (make-string (- (current-column) 6) ?\s)
                        fill-column docsetutil-fill-column)
                  (unwind-protect
                      (fill-region-as-paragraph (point)
                                                (if (re-search-forward field-re limit t)
                                                    (match-beginning 0)
                                                  (point-max)))
                    (setq fill-prefix fp
                          fill-column fc)))))
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
            (setq missing (not (file-exists-p path)))
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

;;;###autoload
(defun docsetutil-search (term &optional full-text)
  "Use `docsetutil' to search documentation on TERM.
With prefix, also include full text search results."
  (interactive
   (list (completing-read
          (format "Apple docset %s search (default: %s): "
                  (if current-prefix-arg "full text" "API")
                  (current-word))
          (docsetutil-objc-completions)
          nil nil nil 'docsetutil-search-history (current-word))
         current-prefix-arg))
  ;; Strip leading and trailing blank chars
  (when (string-match "^[ \t\n]*\\(.*?\\)[ \t\n]*$" term)
    (setq term (match-string 1 term)))
  ;; For API search, space is used to separate terms i.e. 'nsstring
  ;; nsnumber' returns results for both NSString and NSNumber.
  (let ((api (with-output-to-string
               (call-process docsetutil-program nil standard-output nil
                             "search" "-skip-text" "-verbose" "-query"
                             term docsetutil-docset-path))))
    (if (and (not full-text)
             (not (string-match-p "[ \t]+" term))
             (string-match "^Found total of 0 API matches in.*$" api))
        (message "%s" (match-string 0 api))
      (help-setup-xref (list #'docsetutil-search term full-text)
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (princ api)
        (when full-text
          (princ "Full text search results:\n")
          (apply #'call-process docsetutil-program nil standard-output nil
                 `("search" ,@(and docsetutil-use-text-tree '("-text-tree"))
                   "-skip-api" "-query" ,term ,docsetutil-docset-path))))
      (docsetutil-highlight-search-results (help-buffer))
      (let ((help-window (get-buffer-window (help-buffer))))
        (when help-window
          (fit-window-to-buffer help-window (floor (frame-height) 2)))))))

(provide 'docsetutil)
;;; docsetutil.el ends here
