;;; day5 --- My solution to day5 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day5

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun scan-forward (str i)
  "Scan forward in STR from I looking for a non ?_ char.

Produce that index."
  (cl-position-if-not (apply-partially #'eq ?_) str :start (1+ i)))

(defun faster-react (str)
  "React the reacting character pairs in STR by setting them to ?_.

Produce a tuple which indicates whether we removed any."
  (cl-loop while (< i (1- (length str)))
     with i           = 0
     with did-collide = nil
     for  char        = (aref str i)
     if (not (eq char ?_))
       do (let ((next-idx  (scan-forward str i)))
            (when next-idx
              (let* ((next-char (aref str next-idx))
                     (collided  (and (not (eq char next-char))
                                     (string-equal (downcase (char-to-string char))
                                                   (downcase (char-to-string next-char))))))
                (when collided
                  (setq did-collide t)
                  (aset str i        ?_)
                  (aset str next-idx ?_)
                  (cl-decf i 2)
                  (when (< i 0)
                    (setq i -1))))))
     do (cl-incf i)
     finally return did-collide))

(defun react (str)
  "React the reacting character pairs in STR."
  (cl-loop while (< i (length str))
     with i = 0
     for char = (aref str i)
     for collided = (and (< i (1- (length str)))
                         (not (eq char (aref str (1+ i))))
                         (string-equal (downcase (char-to-string char))
                                       (downcase (char-to-string (aref str (1+ i))))))
     when (not collided)
       concat (char-to-string char)
     when collided
       do (cl-incf i)
     do (cl-incf i)))

(defun day5-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let ((converted (if (vectorp input-file) input-file (cl-map 'vector #'identity input-file))))
    (cl-loop
       for  did-collide = (faster-react converted)
       until (not did-collide)
       finally return (cl-count-if-not (apply-partially #'eq ?_) converted))))

;; Correct! with length: 9808 (done on slower react...)

;; # PART 2:

(defun remove-both-cases (c str)
  "Remove both lower and upper case C from STR."
  (let* ((c-str (char-to-string c))
         (lower (aref (downcase c-str) 0))
         (upper (aref (upcase   c-str) 0)))
    (cl-remove-if (lambda (x) (or (eq x lower)
                                  (eq x upper)))
                  str)))

(defun day5-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop for char in '(?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)
     with best-length = (day5-part-1 (remove-both-cases ?a   input-file))
     do (message "Working on %s" (char-to-string char))
     for  next-length = (day5-part-1 (remove-both-cases char input-file))
     when (< next-length best-length)
       do (setq best-length next-length)
     finally return best-length))

;; Answer 6484 (it gives answers which are off by one because of file endings)

;; Run the solution:

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
    (message "Part 2: %s\n" (day5-part-2 input-2))))

(provide 'day5)
;;; day5 ends here
