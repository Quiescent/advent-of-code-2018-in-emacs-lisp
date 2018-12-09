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

(defun remove-seven-back ()
  "Remove and produce the marble in MARBLES which is 7 behind."
  (let ((count 0))
    (beginning-of-line)
    (while (and (not (eq count 7))
                (not (eq (point) (point-min))))
      (forward-line -1)
      (cl-incf count))
    (when (not (eq count 7))
      (goto-char (point-max))
      (forward-line -1)
      (beginning-of-line)
      (dotimes (_ (1- (- 7 count)))
        (forward-line -1)))
    (prog1
      (string-to-number (thing-at-point 'symbol t))
      (kill-line 1))))

(with-temp-buffer
  (mapc (lambda (char) (insert (format "%s\n" char)))
        '(0 1 2 3 4 5 6 7 8 9))
  (goto-line 3)
  (remove-seven-back)
  (format "At: %s, with values: %s"
          (line-number-at-pos (point))
          (buffer-substring (point-min) (point-max))))

(defun move-forward-twice ()
  "Move forwards 1 lines in current buffer.

Rotate to start of buffer when necessary."
  (when (eq (point) (point-max))
    (goto-char (point-min)))
  (forward-line 1)
  (when (eq (point) (point-max))
    (goto-char (point-min)))
  (forward-line 1))

(defun place-marble (current-marble)
  "Place the CURRENT-MARBLE into the marbles buffer."
  (if (eq 0 (mod current-marble 23))
      `(,(remove-seven-back) ,current-marble)
      (progn
        (move-forward-twice)
        (save-excursion
          (insert (format "%s\n" current-marble)))
        '())))

(require 'seq)
(require 'subr-x)

(defun day9-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((current-score  nil)
         (current-marble 1)
         (current-player 1)
         (player-scores (make-hash-table)))
    (with-temp-buffer
      (insert "0\n")
      (beginning-of-line)
      (forward-line -1)
      (pcase (parse-settings input-file)
        (`(,players ,last-score)
          (while (<= current-marble last-score)
            (let ((marbles-received (place-marble current-marble)))
              (setq current-score (apply #'+ marbles-received))
              (cl-incf (gethash current-player player-scores 0)
                       current-score))
            (setq current-player (mod (1+ current-player) players))
            (cl-incf current-marble))
          (seq-max (hash-table-values player-scores)))))))

(let* ((test-input    "10 players; last marble is worth 1618 points")
       (test-computed (day9-part-1 test-input))
       (test-ans      8317))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Solution: 424112

;; # PART 2:

(defun day9-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((current-score  nil)
         (current-marble 1)
         (current-player 1)
         (player-scores (make-hash-table)))
    (with-temp-buffer
      (insert "0\n")
      (beginning-of-line)
      (forward-line -1)
      (pcase (parse-settings input-file)
        (`(,players ,last-score)
          (while (<= current-marble (* 100 last-score))
            (let ((marbles-received (place-marble current-marble)))
              (setq current-score (apply #'+ marbles-received))
              (cl-incf (gethash current-player player-scores 0)
                       current-score))
            (setq current-player (mod (1+ current-player) players))
            (cl-incf current-marble))
          (seq-max (hash-table-values player-scores)))))))

;; Solution: 

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
