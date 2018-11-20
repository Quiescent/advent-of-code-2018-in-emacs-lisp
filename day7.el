;;; day7 --- My solution to day7 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day7

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day7-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  )

;; # PART 2:

(defun day7-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day7-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day7-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day7-part-1 input-1))
    (message "Part 2: %s\n" (day7-part-2 input-2))))

(provide 'day7)
;;; day7 ends here
