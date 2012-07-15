# Emacs Interface to docsetutil on OSX #

## Usage ##
1. Install

    (require 'docsetutil)

2. You may consider binding `docsetutil-search` to a key, for example:

    (define-key help-map "d" 'docsetutil-search) ; C-h d

3. Choose a docset to use:

    M-x docsetutil-choose-docset

4. API search with completion:

    M-x docsetutil-search

5. Full text search:

    C-u M-x docsetutil-search

## Objc Completions ##

Function `docsetutil-objc-completions` provides a decent source for
objc completion. It is not true code completion but extremely fast and
can save you a lot of typing. Improvement expected in this area.

## hippie-exp ##

If you use `hippie-expand`, you may want to add
`try-docsetutil-objc-completions` to
`hippie-expand-try-functions-list`

    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-docsetutil-objc-completions
            try-expand-dabbrev-from-kill
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-whole-kill))

## Notes ##

When you first run `docsetutil-search`, it may take a few seconds to
build the completion table. The results are cached to disk (see
`docsetutil-cache-file`).
