;;; day8 --- My solution to day8 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day8

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun sum-array (xs)
  "Produce the sum of array XS."
  (cl-reduce #'+ xs))

(defun sum-range (numbers start len)
  "Produce the sum of the range of numbers [START, START + LEN) in NUMBERS."
  (sum-array (cl-subseq numbers
                        start
                        (+ start len))))

(defun sum-children-meta-data (n numbers)
  "Sum the metadata for children starting at index N defined at NUMBERS."
  (let ((children (aref numbers n))
        (metadata (aref numbers (1+ n))))
    (if (eq 0 children)
        (let* ((metadata-start (+ n 2))
               (metadata-end   (1- (+ metadata-start metadata))))
          (list metadata-end (sum-range numbers
                                        metadata-start
                                        metadata)))
        (cl-loop for i from 0 below children
           with child-start = (+ n 2)
           with sum = 0
           for (next-end sub-sum) = (sum-children-meta-data child-start numbers)
           do (setq child-start (1+ next-end))
           do (cl-incf sum sub-sum)
           finally return (list (+ next-end metadata)
                                (+ sum (sum-range numbers
                                                  (1+ next-end)
                                                  metadata)))))))

(defun day8-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let ((numbers (cl-map 'vector #'string-to-number (split-string input-file " " t))))
    (cadr (sum-children-meta-data 0 numbers))))

;; Wrong: 227
;; Right: 42951

;; # PART 2:

(defun sum-children-meta-data-advanced (n numbers)
  "Sum the metadata for children starting at index N defined at NUMBERS."
  (let ((children (aref numbers n))
        (metadata (aref numbers (1+ n))))
    (if (eq 0 children)
        (let* ((metadata-start (+ n 2))
               (metadata-end   (1- (+ metadata-start metadata))))
          (list metadata-end (sum-range numbers
                                        metadata-start
                                        metadata)))
        (let* ((metadata-start)
               (child-values
                (cl-loop for i from 0 below children
                   with child-start = (+ n 2)
                   for (next-end sub-sum) = (sum-children-meta-data-advanced
                                                child-start
                                                numbers)
                   do (setq child-start (1+ next-end))
                   do (setq metadata-start (+ 1 next-end))
                   collect sub-sum))
               (metadata-end (+ metadata-start metadata))
               (node-weight
                (cl-loop for i from metadata-start below metadata-end
                   for child-idx = (1- (aref numbers i))
                   when (< child-idx (length child-values))
                     sum (nth child-idx child-values))))
          (list (1- metadata-end) node-weight)))))

(defun day8-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let ((numbers (cl-map 'vector #'string-to-number (split-string input-file " " t))))
    (cadr (sum-children-meta-data-advanced 0 numbers))))

;; Solution: 18568

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day8-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day8-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day8-part-1 input-1))
      (message "Part 2: %s\n" (day8-part-2 input-2)))))

(provide 'day8)
;;; day8 ends here
