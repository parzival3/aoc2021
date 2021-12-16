;;; aoc-lib.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/enrico>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: December 04, 2021
;; Modified: December 04, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/enrico/aoc-lib
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Provides some utility function for opening and reading input files
;;
;;
;;
;;; Code:
;;;
(defalias 'and2 (lambda (a b) (and a b)))

(defun aoc-lib-file-to-list (file-name)
  "Retruns a list of numbers contained in FILE-NAME."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t)))

(defun aoc-lib-file-string (file-name)
  "Retruns a list of numbers contained in FILE-NAME."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
  (buffer-substring-no-properties (point-min) (point-max))))

(defun aoc-lib-bool-string (bool-value)
  "Convert a BOOL-VALUE to a string."
  (if bool-value "1" "0"))

(defun aoc-lib-bool-int (bool-value)
  "Convert a BOOL-VALUE to a int."
  (if bool-value 1 0))

(provide 'aoc-lib)
;;; aoc-lib.el ends here
