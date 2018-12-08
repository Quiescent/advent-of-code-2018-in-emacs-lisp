;;; day19 --- My solution to day19 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day19

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun day19-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  )

(let* ((test-input    "")
       (test-computed (day19-part-1 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day19-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

(let* ((test-input    "")
       (test-computed (day19-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (with-temp-buffer
                     (find-file-literally "day19-part-1")
                     (buffer-substring (point-min)
                                       (point-max))))
          (input-2 (with-temp-buffer
                     (find-file-literally "day19-part-1")
                     (buffer-substring (point-min)
                                       (point-max)))))
      (message "Part 1: %s" (day19-part-1 input-1))
      (message "Part 2: %s\n" (day19-part-2 input-2)))))

(provide 'day19)
;;; day19 ends here
