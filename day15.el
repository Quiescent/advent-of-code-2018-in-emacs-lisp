;;; day15 --- My solution to day15 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day15

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day15-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  )

;; # PART 2:

(defun day15-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day15-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day15-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day15-part-1 input-1))
    (message "Part 2: %s\n" (day15-part-2 input-2))))

(provide 'day15)
;;; day15 ends here
