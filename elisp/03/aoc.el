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
;;;
(defvar aoc-lib "../aoc-lib.el")

(load-file
  (expand-file-name aoc-lib
                    (file-name-directory (buffer-file-name))))

(defun aoc-list-of-char-to-list-of-numbers (list-of-chars)
  "Convert a LIST-OF-CHARS to a list of numbers."
  (mapcar (apply-partially #'+ -48) list-of-chars))

(defun aoc-day3-list-of-string-to-array-of-bytes (list-of-strings)
  "Convert a LIST-OF-STRINGS to a multidimensional array of bites."
  (mapcar (lambda (str)
            (aoc-list-of-char-to-list-of-numbers (string-to-list str)))
          list-of-strings))



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




(defun aoc-calculate-gamma (byte-array)
  "Calculate the gamma value from the BYTE-ARRAY."
  (mapcar (apply-partially 'aoc-is-less-then (length byte-array))
          (seq-reduce 'aoc-sum-column-wise byte-array
                      (make-list (length (car byte-array)) 0))))

(defun aoc-calculate-epsilon (byte-array)
  "Calculate the epsilon value from the BYTE-ARRAY."
  (mapcar #'not (aoc-calculate-gamma byte-array)))

(defun aoc--boolean-list-to-byte-number (boolean-list)
  "Return the byte value from a BOOLEAN-LIST."
  (string-to-number (mapconcat #'aoc-lib-bool-string boolean-list "") 2))

(defun aoc--number-list-to-byte-number (number-list)
  "Return the byte value from a NUMBER-LIST."
  (string-to-number (mapconcat #'number-to-string number-list "") 2))

(defun aoc--boolean-list-to-number-list (boolean-list)
  "Return the list of ints from a BOOLEAN-LIST."
  (mapcar #'aoc-lib-bool-int boolean-list))

(defun aoc--pred (value pos input)
  (eq (nth pos input) value))

(defun aoc--filter-not-matching-value-pos (byte-array pos value)
  "Filters the row of the BYTE-ARRAY that have a different VALUE in POS."
  (seq-filter (apply-partially #'aoc--pred value pos) byte-array))

(defconst aoc-oxygen (make-symbol "aoc-oxyge"))
(defconst aoc-co2 (make-symbol "aoc-co2"))

(defun aoc--get-new-array-support-rating (byte-array pos rating)
  "BYTE-ARRAY POS RATING."
  (let ((value-to-check (if (eq rating 'aoc-oxygen) 1 0))
        (function-to-use (if (eq rating 'aoc-oxygen)
                             #'aoc-calculate-gamma
                           #'aoc-calculate-epsilon)))
    ;; Check for the recursive step
    (if (length= byte-array 2)
        (car (aoc--filter-not-matching-value-pos byte-array pos value-to-check))
      ;; Actual recursion
      (aoc--get-new-array-support-rating
       ;; Create the new byte array
       (aoc--filter-not-matching-value-pos byte-array pos (nth pos (aoc--boolean-list-to-number-list (funcall function-to-use byte-array))))
       (1+ pos) rating))))

(defun aoc-day3-solution (file-name)
  "FILE-NAME."
  (let* ((string-list (aoc-read-file-to-list file-name))
         (byte-array  (aoc-day3-list-of-string-to-array-of-bytes string-list))
         (gamma-rate  (aoc-calculate-gamma byte-array))
         (epsilon-rate (aoc-calculate-epsilon byte-array)))
    (* (aoc--boolean-list-to-byte-number gamma-rate)
       (aoc--boolean-list-to-byte-number epsilon-rate))))

(defun aoc-day3-solution-2 (file-name)
  "FILE-NAME."
  (let* ((string-list (aoc-read-file-to-list file-name))
         (byte-array  (aoc-day3-list-of-string-to-array-of-bytes string-list))
         (oxygen-rating  (aoc--get-new-array-support-rating byte-array 0 'aoc-oxygen))
         (scrubber-rating (aoc--get-new-array-support-rating byte-array 0 'aoc-co2)))
    (* (aoc--number-list-to-byte-number oxygen-rating)
       (aoc--number-list-to-byte-number scrubber-rating))))

(defun aoc-day3-solution-2-debug (file-name)
  "FILE-NAME."
  (let* ((string-list (aoc-read-file-to-list file-name))
         (byte-array  (aoc-day3-list-of-string-to-array-of-bytes string-list))
         (oxygen-rating  (aoc--get-new-array-support-rating byte-array 0 'aoc-oxygen))
         (scrubber-rating (aoc--get-new-array-support-rating byte-array 0 'aoc-co2)))
    (cons oxygen-rating
        scrubber-rating)))

;;; Variable for testing
(defvar aoc-day3-simple-input
  (aoc-read-file-to-list "simple_input.txt"))

(defvar aoc-day3-input
  (aoc-read-file-to-list "input.txt"))

(defvar aoc-byte-array
  (aoc-day3-list-of-string-to-array-of-bytes aoc-day3-simple-input))

(defvar aoc-byte-array-full
  (aoc-day3-list-of-string-to-array-of-bytes aoc-day3-input))

(aoc-read-file-to-list "simple_input.txt")
(aoc-day3-solution "simple_input.txt")
(aoc-day3-solution "input.txt")
(aoc-day3-solution-2 "input.txt")
(aoc-day3-solution-2-debug "input.txt")
(aoc-day3-solution-2 "simple_input.txt")

(provide 'aoc)
;;; aoc.el ends here
