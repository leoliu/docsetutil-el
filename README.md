# Use Cocoa/iOS documentations in emacs #

![emacs-docsetutil-api-doc](http://i.imgur.com/xFM15.png)

## Usage ##
1. Install

    (require 'docsetutil)

2. You may consider binding `docsetutil-api` to a key, for example:

    (define-key help-map "d" 'docsetutil-api)  ; C-h d

3. Choose a docset to use:

    M-x docsetutil-choose-docset

4. API search with completion:

    M-x docsetutil-api

5. API browse

    M-x docsetutil-browse-api

6. Full text search:

    M-x docsetutil-fulltext

## Objc/Swift Completions ##

Function `docsetutil-completions` provides a decent source for
objc/swift completion. It is not true code completion but extremely
fast and can save you a lot of typing. Improvement expected in this
area.

## hippie-exp ##

If you use `hippie-expand`, you may want to add
`try-docsetutil-completions` to `hippie-expand-try-functions-list`

    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-docsetutil-completions
            try-expand-dabbrev-from-kill
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-whole-kill))

## Notes ##

When you first run `docsetutil-api`, `docsetutil-browse-api` or
`docsetutil-fulltext`, it may take a few seconds to build the
completion table, which is then cached to disk in the directory
`docsetutil-cache-directory`.
