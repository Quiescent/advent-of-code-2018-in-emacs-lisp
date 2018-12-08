;;; day1 --- My solution to day1 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day1

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run from batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun day1-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop for line in (split-string input-file "\n" t " ")
     sum (string-to-number line)))

(let* ((test-input    "")
       (test-computed (day1-part-1 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day1-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let (result
        (frequency 0))
    (while (consp (setq result
                        (cl-loop with frequencies-seen = result
                           for line in (split-string input-file "\n" t " ")
                           do (setq frequency (+ frequency (string-to-number line)))
                           when (member frequency frequencies-seen)
                             return frequency
                           do (push frequency frequencies-seen)
                           finally return frequencies-seen))))
    result))

(let* ((test-input    "")
       (test-computed (day1-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (with-temp-buffer
                     (find-file-literally "day1-part-1")
                     (buffer-substring (point-min)
                                       (point-max))))
          (input-2 (with-temp-buffer
                     (find-file-literally "day1-part-1")
                     (buffer-substring (point-min)
                                       (point-max)))))
      (message "Part 1: %s" (day1-part-1 input-1))
      (message "Part 2: %s\n" (day1-part-2 input-2)))))

(provide 'day1)
;;; day1 ends here
