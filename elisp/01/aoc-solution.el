;;; aoc.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Enrico Tolotto
;;
;; Author: Enrico Tolotto <https://github.com/enrico>
;; Maintainer: Enrico Tolotto <etolotto@gmail.com>
;; Created: December 01, 2021
;; Modified: December 01, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/enrico/solution
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;

(defvar aoc-day1-input
  (with-temp-buffer (insert-file-contents-literally "input.txt")
                    (mapcar (lambda (depth) (string-to-number depth))
                              (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))))


(defun aoc--sonar-sweep-count (depth-measurement previous counter)
  "Count the number of times a DEPTH-MEASUREMENT increases by keeping track of the PREVIOUS measure and a COUNTER that tracks the number of times the depth increased."
  (if (not depth-measurement)
      counter
    (let* ((head (car depth-measurement))
           (add (if (< previous head) 1 0)))
      (aoc--sonar-sweep-count (cdr depth-measurement) head (+ counter add)))))

(defun aoc--sonar-sweep-count-2 (depth-measurement)
  "Count the number of times a DEPTH-MEASUREMENT."
  (let ((values (list (cons (car depth-measurement)  0))))
    (while (cdr depth-measurement)
             (let* ((int-depth (pop depth-measurement))
                    (previous-depth (car (car values)))
                    (previous-count (cdr (car values)))
                    (add (if (> int-depth previous-depth) 1 0)))
               (push (cons int-depth (+ previous-count add)) values)))
    (cdr (car values))))

(defun aoc--sonar-sweep-count-second (depth-measurement)
  "Count the number of times a DEPTH-MEASUREMENT is less then before with a sliding window of 3."
  (let ((values (list (cons 0 0))))
    (while (> (length depth-measurement) 3)
              (let* ((int-depth (+ (pop depth-measurement)
                                  (car depth-measurement)
                                  (car (cdr depth-measurement))))
                    (previous-depth (car (car values)))
                    (previous-count (cdr (car values)))
                    (add (if (> int-depth previous-depth) 1 0)))
               (push (cons int-depth (+ previous-count add)) values)))
    (- (cdr (car values)) 1)))


(provide 'aoc)
;;; aoc-solution.el ends here
