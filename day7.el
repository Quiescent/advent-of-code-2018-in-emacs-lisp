;;; day7 --- My solution to day7 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day7

;;; Code:

;; # PART 1:

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
     so that Emacs doesn't hang.")

(require 'cl-lib)

(require 'subr-x)

(defun parse-graph-edges (input-file)
  "Parse the edges in INPUT-FILE."
  (mapcar (lambda (x) (let ((split (split-string x " " t " ")))
                        `(,(nth 1 split) ,(nth 7 split))))
          (split-string input-file "\n" t " ")))

(defun find-roots (edges)
  "Produce the verteces in EDGES which non of the other vertices point to."
  (let ((vertices (cl-remove-duplicates (mapcar #'car edges) :test #'string-equal)))
    (cl-remove-if (lambda (vertex) (cl-member vertex edges :key #'cadr :test #'string-equal))
                  vertices)))

(defun find-next-vertices (vertex edges)
  "Find the next vertices which we should go to from VERTEX in EDGES."
  (cl-mapcar #'cadr (cl-find vertex edges :key #'car)))

(defun day7-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     with ordering           = nil
     with edges              = (parse-graph-edges input-file)
     with vertices-remaining = (cl-remove-duplicates (mapcar #'car edges) :test #'string-equal)
     with initial-vertices   = (cl-union
                                (cl-remove-duplicates (mapcar #'cadr edges) :test #'string-equal)
                                  vertices-remaining :test #'string-equal)
     for  root               = (car (cl-sort (find-roots edges) #'string-lessp))
     until (eq nil vertices-remaining)
     do (setq vertices-remaining (cl-remove root vertices-remaining))
     do (setq ordering           (append ordering (list root)))
     do (setq edges              (cl-remove-if (lambda (x) (equal x root)) edges :key #'car))
     finally return (append ordering (cl-set-difference initial-vertices ordering :test #'string-equal))))

(let* ((test-input    "")
       (test-computed (day7-part-1 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun step-length (char)
  "Produce the step length of CHAR in seconds."
  (1+ (- char ?A)))

(defun day7-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     with second             = 0
     with schedule           = nil
     with available-workers  = '(1 2 3 4 5)
     with ordering           = nil
     with edges              = (parse-graph-edges input-file)
     with initial-vertices   = (cl-union (cl-remove-duplicates (mapcar #'cadr edges) :test #'string-equal)
                                         (cl-remove-duplicates (mapcar #'car edges)  :test #'string-equal)
                                  :test #'string-equal)

     for items-expired = (cl-sort (cl-remove-if-not (lambda (x) (eq x second)) schedule :key #'caddr)
                            #'string-lessp :key #'cadr)
     do (setq schedule (cl-remove-if (lambda (x) (eq x second)) schedule :key #'caddr))
     do (cl-loop for (worker root _) in items-expired
           do (push worker available-workers)
           do (setq edges    (cl-remove-if (lambda (x) (equal x root)) edges :key #'car)
                    ordering (append ordering (list root))))
     for  roots = (cl-sort (cl-set-difference (find-roots edges)
                                              (mapcar #'cadr schedule)
                              :test #'string-equal)
                     #'string-lessp)
     until (eq (length ordering) (1- (length initial-vertices)))

     do (cl-mapc (lambda (worker
                          root)
                   (progn
                     (push `(,worker ,root ,(+ second
                                               60
                                               (step-length (aref root 0))))
                           schedule)
                     (setq available-workers (remove worker available-workers))))
           available-workers roots)
     do (setq schedule (cl-sort schedule #'< :key #'caddr))
     do (cl-incf second)
     finally return (+ second 60 (step-length (aref
                                               (car
                                                (cl-set-difference initial-vertices
                                                                   ordering
                                                                   :test #'string-equal))
                                               0)))))

(let* ((test-input    "")
       (test-computed (day7-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Wrong solution: OPCUEXFHIRGWDZABTQYJMNKVSL
;; Another:        OPCUXEFHIRWGZDABTQYJMNKVSL
;; Another:        OPCUXEHFIRWZGDABTQYJMNKVSL
;; It's the length (duh!)
;; Wrong solution: 1198
;; (had the wrong number of workers from testing :(  )
;; Another

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day7-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day7-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day7-part-1 input-1))
      (message "Part 2: %s\n" (day7-part-2 input-2)))))

(provide 'day7)
;;; day7 ends here


