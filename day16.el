;;; day16 --- My solution to day16 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day16

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

(defun group-into-threes (xs)
  "Group the elements of XS into triples."
  (cl-loop for xt on xs by #'cdddr
     collect (cl-subseq xt 0 3)))

(defun split-on-non-numeric (str)
  "Split STR on non-numeric characters and eliminate nulls."
  (mapcar #'string-to-number (split-string str "[^0-9]" t " ")))

(require 'subr-x)

(defun parse-triple (triple)
  "Parse the TRIPLE into a structured triple.

With (registers, instruction, registers) of type array, list
array respectively."
  (pcase triple
    (`(,before ,inst ,after)
      (list (thread-last (split-on-non-numeric before)
              (apply #'vector ))
            (split-on-non-numeric inst)
            (thread-last (split-on-non-numeric after)
              (apply #'vector))))))

(defun number-of-matching-instructions (scenario)
  "Produce the number of instructions which could be the instruction in SCENARIO."
  (pcase scenario
    (`(,registers (,_ ,a ,b ,c) ,after)
      (cl-loop for instruction across instructions
         for register-copy   = (cl-subseq registers 0)
         do (funcall instruction a b c register-copy)
         count (equal register-copy after)))))

(defun day16-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((parts     (split-string input-file "\n\n\n\n"))
         (lines     (split-string (car parts) "\n" t " "))
         (scenarios (thread-last (group-into-threes lines)
                      (mapcar #'parse-triple))))
    (cl-loop for scenario in scenarios
       count (>= (number-of-matching-instructions scenario) 3))))

(let* ((test-input    "Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]



12323")
       (test-computed (day16-part-1 test-input))
       (test-ans      1))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun matching-instructions (scenario)
  "Produce the number of the instructions which could be the instruction in SCENARIO."
  (pcase scenario
    (`(,registers (,i ,a ,b ,c) ,after)
      (cl-loop for instruction being the elements of instructions
         using (index l)
         with matching      = '()
         for  register-copy = (cl-subseq registers 0)
         do (funcall instruction a b c register-copy)
         when (equal register-copy after)
           do (push l matching)
         finally return (cons i matching)))))

(defun dudup-values (table)
  "Produce TABLE with value lists dedupped."
  (cl-loop for key in (hash-table-keys table)
     do (puthash key (cl-remove-duplicates (gethash key table)) table)
     finally return table))

(defun group-matching (scenarios)
  "Produce a grouping of instruction in their program to potential instructions in mine.

SCENARIOS determine the matches."
  (let* ((match-list (mapcar #'matching-instructions scenarios)))
    (cl-loop for num-matches in match-list
       for  num      = (car num-matches)
       for  matches  = (cdr num-matches)
       with grouping = (make-hash-table :test #'equal)
       do (cl-loop for match in matches
             do (push match (gethash num grouping '())))
       finally return (dudup-values grouping))))

(defun map-registers (scenarios)
  "Produce a mapping from register index in SCENARIOS to my own index."
  (let* ((grouping (group-matching scenarios))
         (mapping  (make-vector    16 nil)))
    (cl-loop while (a-non-singleton-remains grouping)
       for their-idx = (find-singleton-grouping grouping)
       for my-idx    = (car (gethash their-idx grouping))
       do (remove-from-groupings my-idx grouping)
       do (aset mapping their-idx my-idx)
       finally return mapping)))

(defun a-non-singleton-remains (table)
  "Produce t if there is a list in the values of TABLE longer than 1."
  (cl-find-if (lambda (xs) (> (length xs) 0))
              (hash-table-keys table)
              :key (lambda (key) (gethash key table))))

(defun remove-from-groupings (x table)
  "Remove the value X from all lists in the values of TABLE."
  (cl-loop for key in (hash-table-keys table)
     for xs = (gethash key table)
     do (puthash key (remove x xs) table)))

(defun find-singleton-grouping (table)
  "Produce the first key in TABLE which points to list of length 1."
  (cl-find-if (lambda (xs) (eq (length xs) 1))
              (hash-table-keys table)
              :key (lambda (key) (gethash key table))))

(defun day16-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((parts     (split-string input-file "\n\n\n\n"))
         (lines     (split-string (car parts) "\n" t " "))
         (scenarios (thread-last (group-into-threes lines)
                      (mapcar #'parse-triple)))
         (mapping   (map-registers scenarios))
         (program   (thread-last (thread-first (cadr parts)
                                   (split-string "\n" t " "))
                      (mapcar #'split-on-non-numeric))))
    (cl-loop for loc in program
       with registers           = (make-vector 4 0)
       for  (instruction a b c) = loc
       for  idx                 = (aref mapping instruction)
       do (funcall (aref instructions idx) a b c registers)
       finally return (progn
                        (message "%s" mapping)
                        (aref registers 0)))))

;; Too high: 553

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day16-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day16-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day16-part-1 input-1))
      (message "Part 2: %s\n" (day16-part-2 input-2)))))

(provide 'day16)
;;; day16 ends here
