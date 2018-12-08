;;; day5 --- My solution to day5 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day5

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun chars-react ()
  "Produce t if the char at point reacts with the next."
  (let ((this-char (aref (thing-at-point 'char t) 0))
        (next-char (aref (ignore-errors
                           (save-excursion
                             (forward-char)
                             (thing-at-point 'char t)))
                         0)))
    (and (not (eq this-char next-char))
         (eq (downcase this-char)
             (downcase next-char)))))

(defun day5-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (with-temp-buffer
    (insert input-file)
    (goto-char (point-min))
    (while (not (eq (point) (1- (point-max))))
      (if (chars-react)
          (progn (delete-char 2)
                 (ignore-errors (backward-char)))
          (forward-char)))
    (length (buffer-substring (point-min) (point-max)))))

(let* ((test-input    "dabAcCaCBAcCcaDA")
       (test-computed (day5-part-1 test-input))
       (test-ans      (length "dabCBAcaDA")))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Correct! with length: 9808 (done on slower react...)

;; # PART 2:

(require 'seq)

(defun day5-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (seq-min
   (mapcar
    (lambda (char) (day5-part-1 (cl-remove (upcase char)
                                           (cl-remove (downcase char)
                                                      input-file))))
    '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z))))

(let* ((test-input    "dabAcCaCBAcCcaDA")
       (test-computed (day5-part-2 test-input))
       (test-ans      4))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Answer 6484 (it gives answers which are off by one because of file endings)

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day5-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day5-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day5-part-1 input-1))
      (message "Part 2: %s\n" (day5-part-2 input-2)))))

(provide 'day5)
;;; day5 ends here
