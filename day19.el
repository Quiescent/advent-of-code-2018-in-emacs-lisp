;;; day19 --- My solution to day19 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day19

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

(defun day19-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((pnt       0)
         (registers (make-vector 6 0)))
    (pcase (parse-program input-file)
      (`(,program . ,pnt-reg)
        (let* ((len (length program)))
          (cl-loop while (and (>= pnt 0) (< pnt len))
             do (aset registers pnt-reg pnt)
             for (inst a b c) = (aref program pnt)
             do (funcall inst a b c registers)
             do (setq pnt (aref registers pnt-reg))
             do (cl-incf pnt)
             finally return (aref registers 0)))))))

(let* ((test-input    "#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5")
       (test-computed (day19-part-1 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day19-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((pnt       0)
         (registers (make-vector 6 0)))
    (aset registers 0 1)
    ;; (pcase (parse-program input-file)
    ;;   (`(,program . ,pnt-reg)
    ;;     (let* ((len (length program)))
    ;;       (cl-loop while (and (>= pnt 0) (< pnt len))
    ;;          do (aset registers pnt-reg pnt)
    ;;          for (inst a b c) = (aref program pnt)
    ;;          do (funcall inst a b c registers)
    ;;          do (message "%s -- %s -- %s" pnt registers (aref program pnt))
    ;;          when (not (eq pnt (aref registers pnt-reg)))
    ;;            do (message "%s -> %s -- %s"
    ;;                        pnt
    ;;                        (aref registers pnt-reg)
    ;;                        registers)
    ;;          do (setq pnt (aref registers pnt-reg))
    ;;          do (cl-incf pnt)
    ;;          finally return (aref registers 0)))))
    (apply #'+
           (cl-loop for l from 1 to 10551367
              when (eq (mod 10551367 l) 0)
                collect l))))


;; 2 is incorrect and also 1 :/

;; 4 is incorrect

;; 55665678060028 is too high...

;; Program sums the values from 1 to 10551367
;; Result = (10551367 * (10551367 + 1)) / 2

;; It's not the sum, it's the sum of the numbers up to the number of
;; evenly divisible numbers up to 10551367.

;; No it's the sum of the evenly divisible numbers!!!

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day19-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day19-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day19-part-1 input-1))
      (message "Part 2: %s\n" (day19-part-2 input-2)))))

(provide 'day19)
;;; day19 ends here
