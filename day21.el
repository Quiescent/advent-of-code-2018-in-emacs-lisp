;;; day21 --- My solution to day21 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day21

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun addr (a b c registers)
  (aset registers c (+ (aref registers a) (aref registers b))))

(defun addi (a b c registers)
  (aset registers c (+ (aref registers a) b)))

(defun mulr (a b c registers)
  (aset registers c (* (aref registers a) (aref registers b))))

(defun muli (a b c registers)
  (aset registers c (* (aref registers a) b)))

(defun banr (a b c registers)
  (aset registers c (logand (aref registers a) (aref registers b))))

(defun bani (a b c registers)
  (aset registers c (logand (aref registers a) b)))

(defun borr (a b c registers)
  (aset registers c (logior (aref registers a) (aref registers b))))

(defun bori (a b c registers)
  (aset registers c (logior (aref registers a) b)))

(defun setr (a _ c registers)
  (aset registers c (aref registers a)))

(defun seti (a _ c registers)
  (aset registers c a))

(defun gtir (a b c registers)
  (aset registers c (if (> a (aref registers b)) 1 0)))

(defun gtri (a b c registers)
  (aset registers c (if (> (aref registers a) b) 1 0)))

(defun gtrr (a b c registers)
  (aset registers c (if (> (aref registers a) (aref registers b)) 1 0)))

(defun eqir (a b c registers)
  (aset registers c (if (eq a (aref registers b)) 1 0)))

(defun eqri (a b c registers)
  (aset registers c (if (eq (aref registers a) b) 1 0)))

(defun eqrr (a b c registers)
  (aset registers c (if (eq (aref registers a) (aref registers b)) 1 0)))

(defconst instructions [addr addi
                             mulr muli
                             banr bani
                             borr bori
                             setr seti
                             gtir gtri gtrr
                             eqir eqri eqrr]
  "All Possible instructions.")

(require 'subr-x)

(defun decode (instr)
  "Decode the given INSTR to a function."
  (intern instr))

(defun parse-program (input-file)
  "Produce a program as a vector from INPUT-FILE."
  (let* ((lines (split-string input-file "\n" t " ")))
    (cons (cl-map 'vector
                  (lambda (line) (let* ((tokens      (split-string line " " t))
                                        (instruction (car tokens))
                                        (numbers     (thread-last (cl-subseq tokens 1)
                                                       (cl-mapcar #'string-to-number))))
                                   (cons (decode instruction) numbers)))
                  (cdr lines))
          (thread-last (aref (car lines) (1- (length (car lines))))
            (char-to-string)
            (string-to-number)))))

(defun day21-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((registers (make-vector 6 0)))
    (pcase (parse-program input-file)
      (`(,program . ,pnt-reg)
        (let* ((len (length program)))
          (thread-first (cl-loop for i from 0 to 0
                           for pnt           = 0
                           for terminated-at = (cl-loop while (and (>= pnt 0) (< pnt len))
                                                  do (message "%s -- %s" pnt registers)
                                                  with cnt = 0
                                                  when (> cnt 10000)
                                                    return nil
                                                  do (aset registers pnt-reg pnt)
                                                  for (inst a b c) = (aref program pnt)
                                                  do (funcall inst a b c registers)
                                                  do (setq pnt (aref registers pnt-reg))
                                                  do (cl-incf pnt)
                                                  do (cl-incf cnt)
                                                  finally return cnt)
                           when terminated-at
                             collect (cons i terminated-at))
            (cl-sort #'< :key #'car)
            (car)))))))

;; Answer: 13443200 -- it was the number in e which should equal a
;; when we do the jump to 28.  See the cpp file for the day to see a
;; more legible version of the program.

;; # PART 2:

;; I need a value which is less than 16777216 for which d < 256.
;;
;; d is set as e | 65536 (10000000000000000) which means that d will
;; always have that bit set
;;
;; e is 10283511 + n * b where b is a number such that (b + 1)^2 > d
;; when this happens, then d becomes b

;; I think that the last escape can happen when b = 255.
;; At this time

;; Actually, how about translating it and seeing whether I can make it
;; fast enough.

;; This prints out the registers.  I'm looking for the repeat in the
;; pattern.  I'll be the value of e before the e's repeat.
(defun day21-part-2 (_)
  "Run my solution to part two of the problem."
  (let ((a 0)
        (b 0)
        (c 5)
        (d 0)
        (e 0)
        (f 0)
        (max-lisp-eval-depth 15000)
        (max-specpdl-size    60000))
    ;; Line6 onwards
    (cl-block blah
      (cl-labels ((line6 () (progn
                              (message "6  -- %s" (list a b c d e f))
                              (setq d (logior e 65536)
                                    e 10283511
                                    b (mod d 256)
                                    e (+ e b)
                                    e (mod e 16777216)
                                    e (* e 65899)
                                    e (mod e 16777216))
                              (if (< d 256)
                                  #'line28
                                  #'line17)))
                  (line8  () (progn
                               (message "8  -- %s" (list a b c d e f))
                               (setq b (mod d 256)
                                     e (+ e b)
                                     e (mod e 16777216)
                                     e (* e 65899)
                                     e (mod e 16777216))
                               (if (< d 256)
                                   #'line28
                                   #'line17)))
                  (line17 () (progn
                               (message "17 -- %s" (list a b c d e f))
                               (setq d (/ d 256)
                                     b d)
                               (message "product: %s" (* (1+ b) 256))
                               (if (> (* (1+ b) 256) 2147483647)
                                   (cl-return)
                                   #'line26)))
                  (line26 () (progn
                               (message "26 -- %s" (list a b c d e f))
                               (setq d b)
                               #'line8))
                  (line28 () (progn
                               (message "28 -- %s" (list a b c d e f))
                               (if (eq e a)
                                   (cl-return)
                                   #'line6))))
        (let* ((next-func #'line6))
          (while t
            (setq next-func (funcall next-func))))))))

;; Maximum value of d for which we can still escape: 8388606
;; b = 8388606 % 256 = 254
;; e = e + 254

;; So I found the solution by running the above function for a long
;; time, cutting the e column when the line of code was 28 and then
;; finding the last element from the bottom which didn't repeat itself
;; above.  (The last step was done by interactively running some elisp
;; from a buffer with the values in it.)
;;
;; The whole thing with the overflow was a wild goose chase :/

;; Answer: 7717135

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day21-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day21-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      ;;(message "Part 1: %s" (day21-part-1 input-1))
      (message "Part 2: %s\n" (day21-part-2 input-2)))))

(provide 'day21)
;;; day21 ends here
