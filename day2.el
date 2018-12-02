;;; day2 --- My solution to day2 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day2

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun letter-content (str)
  "Produce a table of the letter count in STR."
  (let ((table (make-hash-table :test #'eq)))
    (cl-loop for char across str
       for cnt = (gethash char table 0)
       do (puthash char (cl-incf cnt) table)
       finally return table)))

(require 'subr-x)

(defun contains-three-p (table)
  "Produce t if TABLE contains the same letter three times."
  (cl-loop for key in (hash-table-keys table)
     thereis (eq 3 (gethash key table))))

(defun contains-two-p (table)
  "Produce t if TABLE contains the same letter three times."
  (cl-loop for key in (hash-table-keys table)
     thereis (eq 2 (gethash key table))))

(defun day2-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop for line in (split-string input-file "\n" t " ")
     with two-count = 0
     with three-count = 0
     for content = (letter-content line)
     when (contains-three-p content)
       do (cl-incf three-count)
     when (contains-two-p content)
       do (cl-incf two-count)
     finally return (* three-count two-count)))

;; # PART 2:

(defun differ-by-one-p (this other)
  "Produce t if THIS is different from OTHER by one character."
  (cl-loop for i below (length this)
     with count = 0
     when (not (eq (aref this i) (aref other i)))
       do (cl-incf count)
     finally return (eq 1 count)))

(defun day2-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let ((lines (split-string input-file "\n" t " ")))
    (cl-loop for id in lines
       with other-string
       do (cl-loop for other in lines
             when (differ-by-one-p id other)
               do (progn
                    (setq other-string other)
                    (cl-return)))
       when other-string
         return (cl-loop for i below (length id)
                   when (eq (aref id i) (aref other-string i))
                     concat (char-to-string (aref id i))))))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day2-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day2-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day2-part-1 input-1))
    (message "Part 2: %s\n" (day2-part-2 input-2))))

(provide 'day2)
;;; day2 ends here
