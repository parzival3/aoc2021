;;; aoc.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/enrico>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: December 04, 2021
;; Modified: December 04, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/enrico/aoc
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


;;; Dependencies
(require 'cl-lib)

(defvar aoc-lib "../aoc-lib.el")

(load-file (expand-file-name aoc-lib
                    (file-name-directory (buffer-file-name))))

;; Helper function to split the row of a table into numbers
(defalias 'aoc-split-row (lambda (row) (cl-mapcar 'string-to-number (split-string row " " t))))

(defun aoc-read-table (table)
  "Read in memory a TABLE of dimension DIM X DIM."
  (cl-mapcar #'aoc-split-row (split-string table "\n" t)))

;; Helper function to concatenate a list to value -> (4 3) 3 --> (4 3 3).
;; This is necessary because (append '(4) 1) returns '(4 . 1)  instead of (4 1).
(defalias 'aoc-append-list (lambda (prev next) (append prev (list next))))

;; Helper function to concatenate a row to another row.
(defalias 'aoc-append-row (lambda (prev next) (cl-mapcar #'aoc-append-list prev next)))

(defun aoc-t-table (table)
  "Function that transposes a TABLE, a table is an array of numbers."
  ;; We need to loop here unfortunatelly
  (let ((transpose (make-list (length table) ())))
    (seq-reduce #'aoc-append-row table transpose)))

(defun aoc-check-bingo (list-of-number table)
  "Function that check if there is a bingo for the TABLE given a LIST-OF-NUMBER."
  (cl-flet ((check-row  (lambda (row) (cl-set-difference row list-of-number)))
            (and-row    (lambda (x y) (and x y))))
    (not (and (cl-reduce #'and-row (cl-mapcar #'check-row table))
              (cl-reduce #'and-row (cl-mapcar #'check-row (aoc-t-table table)))))))

(defun aoc-if-bingo-calc (list-of-numbers table)
  "Return the result if the TABLE has a bingo with the LIST-OF-NUMBERS."
  (if (aoc-check-bingo list-of-numbers table)
      (* (car list-of-numbers)
         (cl-reduce #'+ (cl-set-difference (flatten-list table) list-of-numbers)))))

;; pop from a the complete list and push to currently check list
;; if a table is in bingo then

(defun aoc-check-current-bingo (list-of-tables pair-new-res new-num)
  ""
  (let ((new-list (append (list new-num) (car pair-new-res))))
    (if (not (eq 0 (cdr pair-new-res)))
        (cons new-list
              (cl-reduce #'aoc-lib-or2 (cl-mapcar (apply-partially #'aoc-if-bingo-calc new-list)
                                         list-of-tables)))
      (cons new-list (cdr pair-new-res)))))

(defun aoc-get-bingo-results (list-of-numbers list-of-tables)
  "Function that return the result of the bingo give a LIST-OF-NUMBERS and a LIST-OF-TABLES."
  (cl-flet ((aoc-bingo (lambda (pair new-num) (aoc-check-current-bingo list-of-tables pair new-num))))
    (cl-reduce #'aoc-bingo list-of-numbers :initial-value (cons nil nil))))

;;; Variables/Inputs
(defvar aoc-simple-table (aoc-lib-file-string "table.txt"))
(defvar aoc-input-file-string (aoc-lib-file-string "input.txt"))
(defvar aoc-splitted-file (split-string aoc-input-file-string "^\n" t))
(defvar aoc-bingo-list (cl-mapcar #'string-to-number (split-string (car aoc-splitted-file) "," t)))
(defvar aoc-list-of-tables (cl-mapcar #'aoc-read-table (cdr aoc-splitted-file)))


;;; Tests
;;;
;;;
(defvar aoc-simple-bingo-list
  (cl-mapcar #'string-to-number
  (split-string "7,4,9,5,11,17,23,2,0,14,21,24" "," t)))

(defvar aoc-simple-bingo-list-2
  (cl-mapcar #'string-to-number
  (split-string "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1" "," t)))

(defvar aoc-elist (list (list ()) (list ()) (list ())))
(defvar aoc-list1 (list 1 2 3))
(defvar aoc-tlist (funcall #'aoc-append-row aoc-elist aoc-list1))
(defvar aoc-tlist2 (funcall #'aoc-append-row aoc-tlist aoc-list1))
(defvar aoc-table (aoc-read-table aoc-simple-table))
(defvar aoc-table-t (aoc-t-table aoc-table))
(aoc-get-bingo-results aoc-bingo-list aoc-list-of-tables)
(provide 'aoc)
;;; aoc.el ends here
