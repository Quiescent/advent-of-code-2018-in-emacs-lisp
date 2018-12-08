;;; day9 --- My solution to day9 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day9

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun day9-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  )

;; # PART 2:

(defun day9-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (with-temp-buffer
                     (find-file-literally "day9-part-1")
                     (buffer-substring (point-min)
                                       (point-max))))
          (input-2 (with-temp-buffer
                     (find-file-literally "day9-part-1")
                     (buffer-substring (point-min)
                                       (point-max)))))
      (message "Part 1: %s" (day9-part-1 input-1))
      (message "Part 2: %s\n" (day9-part-2 input-2)))))

(provide 'day9)
;;; day9 ends here
