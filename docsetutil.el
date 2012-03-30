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

(defvar docsetutil-docset-search-paths
  ;; See: http://goo.gl/jiYPv
  '("/Applications/Xcode.app/Contents/Developer/Documentation/DocSets"
    "/Developer/Documentation/DocSets"
    "~/Library/Developer/Shared/Documentation/DocSets"
    "/Library/Developer/Shared/Documentation/DocSets"
    "/Network/Library/Developer/Shared/Documentation/DocSets"
    "/System/Library/Developer/Shared/Documentation/DocSets")

  "A list of directories where XCode search for docsets.")

(defvar docsetutil-program "docsetutil"
  "Executable for docsetutil.

Normally it resides in one of the following directories:
  /Applications/Xcode.app/Contents/Developer/usr/bin/
or
  /Developer/usr/bin/")

(defun docsetutil-all-docsets ()
  "Return all docsets in `docsetutil-docset-search-paths'."
  (loop for p in docsetutil-docset-search-paths
        when (file-directory-p p)
        append
        ;; Match non "." ".." names
        (loop for dir in (directory-files p t "^\\(?:[^.]\\|\\.[^.]\\)")
              when (file-directory-p dir)
              collect dir)))

(defvar docsetutil-docset-path (car (last (docsetutil-all-docsets)))
  "The docset to use by `docsetutil-search'.")

(defvar docsetutil-browse-url-function 'browse-url)

(defvar docsetutil-search-history nil)
(defconst docsetutil-api-regexp "^ \\(.*?\\)   \\(.*?\\) -- \\(.*\\)$")

;; Note: 1. use /usr/libexec/PlistBuddy or /usr/bin/plutil to convert
;; plist to xml1 format; 2. Emacs comes with a few xml parsers:
;; xml-parse-region and libxml-parse-xml-region but used regexp for
;; simplicity.
(defun docsetutil-get-docset-name (docset)
  "Get BundleName from DOCSET's Info.plist file if present."
  (let ((infofile (expand-file-name "Contents/Info.plist" docset)))
    (when (file-readable-p infofile)
      (with-temp-buffer
        (insert-file-contents infofile)
        (when (and (not (looking-at-p "^bplist")) ; binary plist
                   (search-forward "<key>CFBundleName</key>" nil t))
          (forward-line 1)
          (when (re-search-forward "<string>\\([^<]+\\)</string>"
                                   (line-end-position) t)
            (match-string 1)))))))

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
               for name = (or (docsetutil-get-docset-name docset)
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
    (setq docsetutil-docset-path docset)
    (when (called-interactively-p 'interactive)
      (message "Docset: %s" docset))))

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
           :type 'help-xref))
        ;; process full text results
        (while (re-search-forward "^ [0-9.]+ \\(.*\\)$" nil t)
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
   (list (read-string (format "Apple docset %s search (default %s): "
                              (if current-prefix-arg "full text" "API")
                              (current-word))
                      nil 'docsetutil-search-history (current-word))
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
          (call-process docsetutil-program nil standard-output nil
                        "search" "-skip-api" "-query" term docsetutil-docset-path)))
      (docsetutil-highlight-search-results (help-buffer)))))

(provide 'docsetutil)
;;; docsetutil.el ends here
