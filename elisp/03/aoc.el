;;; aoc.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/enrico>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: December 03, 2021
;; Modified: December 03, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/enrico/aoc
;; Package-Requires: ((emacs "26"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;


(defun aoc-char-to-number (char)
  "Return the number corresponding tho the CHAR."
  (- char 48))

(defun aoc-list-of-char-to-list-of-numbers (list-of-chars)
  "Convert a LIST-OF-CHARS to a list of numbers."
  (mapcar #'aoc-char-to-number list-of-chars))

(defun aoc-day3-list-of-string-to-array-of-bytes (list-of-strings)
  "Convert a LIST-OF-STRINGS to a multidimensional array of bites."
  (mapcar (lambda (str)
            (aoc-list-of-char-to-list-of-numbers (string-to-list str)))
          list-of-strings))

(defun aoc-read-file-to-list (file-name)
  "Retruns a list of numbers contained in FILE-NAME."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t)))

(defun aoc-sum-column-wise (prev next)
  "Sum column wise PREV and NEXT and return the resulting list.
The list must have the same size and they must contain only numbers."
  (let ((result ()))
    (while prev
      (push (+ (pop prev)
               (pop next))
            result))
    (reverse result)))

(defun aoc-is-less-then (to-half-it value)
  "Rurns 1 or 0 if VALUE is grather than half of TO-HALF-IT."
  (> value (/ to-half-it 2)))

(defun aoc-bool-to-string (bool-value)
  "Convert a BOOL-VALUE to a string."
  (if bool-value "1" "0"))

(defun aoc-day3-solution (file-name)
  "FILE-NAME."
  (let* ((string-list (aoc-read-file-to-list file-name))
         (byte-array  (aoc-day3-list-of-string-to-array-of-bytes string-list))
         (gamma-rate  (mapcar (apply-partially 'aoc-is-less-then (length byte-array))
                             (seq-reduce 'aoc-sum-column-wise byte-array (make-list (length (car byte-array)) 0))))
         (epsilon-rate (mapcar #'not gamma-rate)))
         (* (string-to-number (mapconcat #'aoc-bool-to-string gamma-rate "") 2)
            (string-to-number (mapconcat #'aoc-bool-to-string epsilon-rate "") 2))))


(defvar aoc-day3-simple-input
  (aoc-read-file-to-list "simple_input.txt"))


(aoc-read-file-to-list "simple_input.txt")
(aoc-day3-solution "simple_input.txt")
(aoc-day3-solution "input.txt")

(provide 'aoc)
;;; aoc.el ends here
