;;; aoc.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/enrico>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: December 02, 2021
;; Modified: December 02, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/enrico/aoc-solution
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;

(defun aoc-day2-sum-input (old-input new-input)
  "OLD-INPUT NEW-INPUT."
         (cons (+ (car old-input) (car new-input))
               (+ (cdr old-input) (cdr new-input))))

(defun aoc-day2-sum-input-with-aim (old-input new-input)
  "OLD-INPUT NEW-INPUT."
    (list (+ (car old-input) (car new-input))
          (+ (cadr old-input) (cdr new-input))
          (+ (caddr old-input) (* (car old-input) (cdr new-input)))))

(defun aoc-day2-remap-string (input)
  "Takes a INPUT and transforms it to something that we can process."
  (let* ((direction-value (split-string input " "))
         (direction (car direction-value))
         (value (string-to-number (cadr direction-value))))
    (if (string-prefix-p "f" direction)
        (cons 0 value)
      (if (string-prefix-p "d" direction)
        (cons value 0)
        (cons (- value) 0)))))

(defun aoc-open-file (file)
  "Open FILE for input data."
  (with-temp-buffer (insert-file-contents-literally file)
                    (cl-set-difference
                     (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")
                              '("") :test 'equal)))

(defvar aoc-day2-simple-input (aoc-open-file "simple_input.txt"))
(defvar aoc-day2-input (aoc-open-file "input.txt"))

(defun aoc-day2-solution (input)
  "Calc the sulution based on the INPUT."
  (let ((sol (seq-reduce #'aoc-day2-sum-input (mapcar #'aoc-day2-remap-string input) '(0 . 0))))
    (* (car sol) (cdr sol))))


(defun aoc-day2-solution-2 (input)
  "Calc the sulution based on the INPUT."
  (let ((sol (seq-reduce #'aoc-day2-sum-input-with-aim (mapcar #'aoc-day2-remap-string input) '(0 0 0))))
    (* (caddr sol) (caddr sol))))

(provide 'aoc)
;;; aoc.el ends here
