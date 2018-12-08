;;; day22 --- My solution to day22 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day22

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day22-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  )

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

;; # PART 2:

(defun day22-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (with-temp-buffer
                     (find-file-literally "day22-part-1")
                     (buffer-substring (point-min)
                                       (point-max))))
          (input-2 (with-temp-buffer
                     (find-file-literally "day22-part-1")
                     (buffer-substring (point-min)
                                       (point-max)))))
      (message "Part 1: %s" (day22-part-1 input-1))
      (message "Part 2: %s\n" (day22-part-2 input-2)))))

(provide 'day22)
;;; day22 ends here
