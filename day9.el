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

(defun parse-settings (input-file)
  "Parse the game settings from INPUT-FILE."
  (let ((split (split-string input-file " " t)))
    (mapcar #'string-to-number (list (car split)
                                     (nth 6 split)))))

(defun remove-seven-back (marbles len)
  "Produce the marble in MARBLES which is 7 backwards from current position.

LEN is the length of the circular list.

Also produce the new list of marbles."
  (let* ((cons-cell (nthcdr (- len 8) marbles))
         (marble    (cadr cons-cell))
         (new-cons  (setcdr cons-cell (cddr cons-cell))))
    `(,marble ,new-cons)))

(let ((xs '(2 3 4 5 6 7 8 9 0 1)))
  (remove-seven-back (setcdr (last xs) xs) 10))

(defun place-marble (current-marble marbles len)
  "Place the CURRENT-MARBLE into MARBLES at position 2.

LEN is the current length of the list.

MARBLES is assumed to be circular.

Produce the marbles you get to keep (adding them to your score),
the new list of marbles and the new length of the circular list."
  (if (eq 0 (mod current-marble 23))
      (pcase (remove-seven-back marbles len)
        (`(,removed ,marbles-with-removal)
          (list `(,current-marble ,removed)
                marbles-with-removal
                (1- len))))
      (list '()
            (progn
              (setcdr (cdr marbles)
                      (cons current-marble
                            (cddr marbles))))
            (1+ len))))

(require 'seq)
(require 'subr-x)

(defun day9-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let ((current-score  nil)
        (current-marble 1)
        (current-player 1)
        (marbles        '(0))
        (len            1)
        (player-scores (make-hash-table)))
    (setcdr marbles marbles)
    (pcase (parse-settings input-file)
      (`(,players ,last-score)
        (while (<= current-marble last-score)
          (pcase (place-marble current-marble marbles len)
            (`(,marbles-received
               ,new-marbles
               ,new-len)
              (setq len new-len)
              (setq marbles new-marbles)
              (setq current-score (apply #'+ marbles-received))
              (cl-incf (gethash current-player player-scores 0)
                       current-score)))
          (setq current-player (mod (1+ current-player) players))
          (cl-incf current-marble))
        (seq-max (hash-table-values player-scores))))))

(let* ((test-input    "10 players; last marble is worth 1618 points")
       (test-computed (day9-part-1 test-input))
       (test-ans      8317))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))
;; Solution: 424112

;; # PART 2:

(defun day9-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let ((current-score  nil)
        (current-marble 1)
        (current-player 1)
        (marbles        '(0))
        (len            1)
        (player-scores (make-hash-table)))
    (setcdr marbles marbles)
    (pcase (parse-settings input-file)
      (`(,players ,last-score)
        (let ((final-marble (* 100 last-score)))
          (while (<= current-marble final-marble)
            (pcase (place-marble current-marble marbles len)
              (`(,marbles-received
                 ,new-marbles
                 ,new-len)
                (setq len new-len)
                (setq marbles new-marbles)
                (setq current-score (apply #'+ marbles-received))
                (cl-incf (gethash current-player player-scores 0)
                         current-score)))
            (setq current-player (mod (1+ current-player) players))
            (cl-incf current-marble)))
        (seq-max (hash-table-values player-scores))))))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day9-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day9-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day9-part-1 input-1))
      (message "Part 2: %s\n" (day9-part-2 input-2)))))

(provide 'day9)
;;; day9 ends here
