;;; day14 --- My solution to day14 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day14

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun append-number (buff len number)
  "Append to BUFF of length LEN, the digits of NUMBER."
  (cl-loop for digit across (number-to-string number)
     do (aset buff len (string-to-number (char-to-string digit)))
     do (cl-incf len)
     finally return len))

(defun move-elf (idx score len)
  "Move elf forward from IDX by SCORE + 1 in a circular buffer of length LEN."
  (mod (+ idx score 1) len))

(defun day14-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((max-len (string-to-number input-file))
         (buff    (make-vector (* 3 max-len) nil))
         (pos-1   0)
         (pos-2   1)
         (len     2))
    (aset buff 0 3)
    (aset buff 1 7)
    (cl-loop
       while (< len (+ 10 max-len))
       for elf1-number = (aref buff pos-1)
       for elf2-number = (aref buff pos-2)
       for sum         = (+ elf1-number elf2-number)
       do (setq len (append-number buff len sum))
       do (setq pos-1 (move-elf pos-1 elf1-number len))
       do (setq pos-2 (move-elf pos-2 elf2-number len))
       finally return (cl-subseq buff max-len (+ 10 max-len)))))

(let* ((test-input    "9")
       (test-computed (day14-part-1 test-input))
       (test-ans      [5 1 5 8 9 1 6 7 7 9]))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day14-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((max-len  (string-to-number input-file))
         (term     (cl-map 'vector
                           (lambda (char)
                             (string-to-number (char-to-string char)))
                           input-file))
         (term-len (length term))
         (buff     (make-vector (* 50 max-len) nil))
         (pos-1    0)
         (pos-2    1)
         (len      2))
    (aset buff 0 3)
    (aset buff 1 7)
    (cl-loop
       while (< len (* 50 max-len))
       for elf1-number = (aref buff pos-1)
       for elf2-number = (aref buff pos-2)
       for sum         = (+ elf1-number elf2-number)
       do (setq len (append-number buff len sum))
       do (setq pos-1 (move-elf pos-1 elf1-number len))
       do (setq pos-2 (move-elf pos-2 elf2-number len))
       for pos-hit = (cl-search term buff
                        :start2 (floor-to-zero (- len term-len 2))
                        :end2 len)
       when pos-hit
         return pos-hit)))

(defun floor-to-zero (x)
  "Produce 0 if X is less than zero, otherwise X."
  (if (< x 0) 0 x))

(let* ((test-input    "59414")
       (test-computed (day14-part-2 test-input))
       (test-ans      2018))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day14-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day14-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day14-part-1 input-1))
      (message "Part 2: %s\n" (day14-part-2 input-2)))))

(provide 'day14)
;;; day14 ends here
