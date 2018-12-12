;;; day12 --- My solution to day12 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day12

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun parse-initial-state (state-line)
  "Parse the initial state from STATE-LINE."
  (cl-map 'vector #'identity (substring state-line 15)))

(require 'subr-x)

(defun parse-rules (rule-lines)
  "Parse rules from RULE-LINES."
  (thread-last (mapcar (lambda (line) (mapcar (lambda (elem) (cl-map 'vector
                                                                     #'identity
                                                                     elem))
                                              (split-string line "=>" t " ")))
                       rule-lines)
    (cl-remove-if (lambda (rule) (eq ?. (aref (cadr rule) 0))))))

(defun parse-initial-state-and-rules (input-file)
  "Produce the initial state and rules from INPUT-FILE."
  (let ((lines (split-string input-file "\n" t " ")))
    `(,(parse-initial-state (car lines))
       ,(parse-rules (cdr lines)))))

(defconst most-left-index -1000
  "The most left we can go in the simulation.")

(defun sum-occupied (state)
  "Sum the indices of the occupied slots in STATE."
  (cl-loop for i from most-left-index below (+ (length state) most-left-index)
     when (eq ?# (aref-centered state i))
       sum i))

(defun aref-centered (arr i)
  "Index into ARR at I as though it were centered at `most-left-index'."
  (aref arr (- i most-left-index)))

(defun apply-rule (state i rules new-state)
  "Apply a rule to STATE at I using RULES stick the results into NEW-STATE."
  (when (cl-loop for (match result) in rules
           when (cl-search match state :start2 (- i 2) :end2 (+ i (length match) -2))
             do (progn
                  (aset new-state i (aref result 0))
                  (cl-return nil))
           finally return t)
    (aset new-state i ?.)))

(defun update-state (state rules)
  "Update STATE according to RULES."
  (cl-loop for i from 2 below (- (length state) 2)
     with new-state = (cl-map 'vector #'identity state)
     do (apply-rule state i rules new-state)
     finally return new-state))

(defun day12-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop repeat 20
     with (initial-state rules) = (parse-initial-state-and-rules input-file)
     with state = (cl-concatenate 'vector
                                   (make-vector (- 0 most-left-index) ?.)
                     initial-state
                     (make-vector (- 0 most-left-index) ?.))
     for next-state = (update-state state rules)
     do (setq state next-state)
     finally return (sum-occupied state)))

;; (let* ((test-input    "initial state: #..#.#..##......###...###

;; ...## => #
;; ..#.. => #
;; .#... => #
;; .#.#. => #
;; .#.## => #
;; .##.. => #
;; .#### => #
;; #.#.# => #
;; #.### => #
;; ##.#. => #
;; ##.## => #
;; ###.. => #
;; ###.# => #
;; ####. => #")
;;        (test-computed (day12-part-1 test-input))
;;        (test-ans      325))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))
;; Solution: 4110

;; # PART 2:

(defun gradients-match (xs)
  "Produce the difference between (car XS) and (cdr XS) when its the same as the following."
  (pcase `(,(nth 0 xs)
            ,(nth 1 xs)
            ,(nth 2 xs)
            ,(nth 3 xs)
            ,(nth 4 xs)
            ,(nth 5 xs)
            ,(nth 6 xs)
            ,(nth 7 xs))
    ('(nil nil nil nil nil nil nil nil) nil)
    (`(,_  nil nil nil nil nil nil nil) nil)
    (`(,_  ,_  nil nil nil nil nil nil) nil)
    (`(,_  ,_  ,_  nil nil nil nil nil) nil)
    (`(,_  ,_  ,_  ,_  nil nil nil nil) nil)
    (`(,_  ,_  ,_  ,_  ,_  nil nil nil) nil)
    (`(,_  ,_  ,_  ,_  ,_  ,_  nil nil) nil)
    (`(,_  ,_  ,_  ,_  ,_  ,_  ,_  nil) nil)
    (`(,a ,b ,u ,v ,w ,x ,y ,z)
      (let ((diff-1 (- b a))
            (diff-2 (- u b))
            (diff-3 (- v u))
            (diff-4 (- w v))
            (diff-5 (- x w))
            (diff-6 (- y x))
            (diff-7 (- z y)))
        (when (and (eq diff-1 diff-2)
                   (eq diff-2 diff-3)
                   (eq diff-3 diff-4)
                   (eq diff-4 diff-5)
                   (eq diff-5 diff-6)
                   (eq diff-6 diff-7))
          diff-1)))))

(defun day12-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop for i from 1 to 1000
     with (initial-state rules) = (parse-initial-state-and-rules input-file)
     with state = (cl-concatenate 'vector
                                   (make-vector (- 0 most-left-index) ?.)
                     initial-state
                     (make-vector (- 0 most-left-index) ?.))
     with sums = `(,(sum-occupied state))
     for next-state = (update-state state rules)
     for next-sum = (sum-occupied state)
     do (push next-sum sums)
     for gradient = (gradients-match sums)
     when gradient
       return (progn
                (message "%s,%s,%s" next-sum gradient i)
                (+ next-sum (* (- 50000000000 (1- i)) (- 0 gradient))))
     do (setq state next-state)
     finally (message "Failed to find gradient.")))

;; We cannot possibly compute the sum for the 50000000000.  The trick
;; must be that the series stabalises.  I'm going to try a few
;; different inputs for the padding to see whether I can find the
;; number by trial and error.

;; Too low: 2650000000413
;; I was off by one...
;; Right answer: 2650000000466

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day12-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day12-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day12-part-1 input-1))
      (message "Part 2: %s\n" (day12-part-2 input-2)))))

(provide 'day12)
;;; day12 ends here
