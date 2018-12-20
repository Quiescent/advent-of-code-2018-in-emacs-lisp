;;; day20 --- My solution to day20 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day20

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(require 'map)

(defun regex-to-graph (regex graph)
  "Parse REGEX and create GRAPH."
  (cl-loop for char being the elements of regex
     using (index pos)
     with stack  = '()
     with next-x = 0
     with next-y = 0
     with x      = 0
     with y      = 0
     do (pcase char
          (?N  (progn
                 (setq next-x x
                       next-y (1- y))
                 (setf (map-elt graph (cons x y) '())
                       (cl-union (map-elt graph (cons x y) '())
                                 (list (cons next-x next-y))))
                 (setf (map-elt graph (cons next-x next-y) '())
                       (cl-union (map-elt graph (cons next-x next-y) '())
                                 (list (cons x y))))
                 (setq x next-x
                       y next-y)))
          (?E  (progn
                 (setq next-x (1+ x)
                       next-y y)
                 (setf (map-elt graph (cons x y) '())
                       (cl-union (map-elt graph (cons x y) '())
                                 (list (cons next-x next-y))))
                 (setf (map-elt graph (cons next-x next-y) '())
                       (cl-union (map-elt graph (cons next-x next-y) '())
                                 (list (cons x y))))
                 (setq x next-x
                       y next-y)))
          (?S  (progn
                 (setq next-x x
                       next-y (1+ y))
                 (setf (map-elt graph (cons x y) '())
                       (cl-union (map-elt graph (cons x y) '())
                                 (list (cons next-x next-y))))
                 (setf (map-elt graph (cons next-x next-y) '())
                       (cl-union (map-elt graph (cons next-x next-y) '())
                                 (list (cons x y))))
                 (setq x next-x
                       y next-y)))
          (?W  (progn
                 (setq next-x (1- x)
                       next-y y)
                 (setf (map-elt graph (cons x y) '())
                       (cl-union (map-elt graph (cons x y) '())
                                 (list (cons next-x next-y))))
                 (setf (map-elt graph (cons next-x next-y) '())
                       (cl-union (map-elt graph (cons next-x next-y) '())
                                 (list (cons x y))))
                 (setq x next-x
                       y next-y)))
          (?\( (push (cons x y) stack))
          (?\) (if (eq (aref regex (1- pos)) ?|)
                   (let ((new-coord (pop stack)))
                     (setq x (car new-coord))
                     (setq y (cdr new-coord)))
                   (pop stack)))
          (?|  (let ((new-coord (car stack)))
                 (setq x (car new-coord))
                 (setq y (cdr new-coord))))
          (?$  nil)
          (?^  nil))))

(require 'seq)
(require 'subr-x)

(defun longest-path (graph)
  "Produce the longest path through GRAPH."
  (let* ((coord        (cons 0 0))
         (seen         (make-hash-table :test #'equal))
         (remaining    (mapcar (lambda (coord) (cons 1 coord))
                               (progn
                                 (setf (map-elt seen coord) 1)
                                 (map-elt graph coord)))))
    (setf (map-elt seen coord) 0)
    (cl-loop
       when (and (not remaining) next-remaining)
         do (progn
              (setq remaining      next-remaining
                    next-remaining '()))
       while remaining
       with  next-remaining  = '()
       for   length-and-next = (pop remaining)
       for   length          = (car length-and-next)
       for   next            = (cdr length-and-next)
       do (cl-loop for destination in (map-elt graph next)
             when (not (map-contains-key seen destination))
               do (progn
                    (setf (map-elt seen destination) (1+ length))
                    (push (cons (1+ length) destination) next-remaining))))
    (thread-last (map-values seen)
      (seq-max))))

(defun day20-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((graph               (make-hash-table :test #'equal))
         (max-lisp-eval-depth 10000)
         (max-specpdl-size    30000))
    (regex-to-graph input-file graph)
    (longest-path graph)))

(let* ((test-input    "^WNE$")
       (test-computed (day20-part-1 test-input))
       (test-ans      3))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "^ENWWW(NEEE|SSE(EE|N))$")
       (test-computed (day20-part-1 test-input))
       (test-ans      10))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
       (test-computed (day20-part-1 test-input))
       (test-ans      18))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
       (test-computed (day20-part-1 test-input))
       (test-ans      23))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
       (test-computed (day20-part-1 test-input))
       (test-ans      31))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Too low: 941

;; # PART 2:

(defun count-over-1000 (graph)
  "Produce the longest path through GRAPH."
  (let* ((coord        (cons 0 0))
         (seen         (make-hash-table :test #'equal))
         (remaining    (mapcar (lambda (coord) (cons 1 coord))
                               (progn
                                 (setf (map-elt seen coord) 1)
                                 (map-elt graph coord)))))
    (setf (map-elt seen coord) 0)
    (cl-loop
       when (and (not remaining) next-remaining)
         do (progn
              (setq remaining      next-remaining
                    next-remaining '()))
       while remaining
       with  next-remaining  = '()
       for   length-and-next = (pop remaining)
       for   length          = (car length-and-next)
       for   next            = (cdr length-and-next)
       do (cl-loop for destination in (map-elt graph next)
             when (not (map-contains-key seen destination))
               do (progn
                    (setf (map-elt seen destination) (1+ length))
                    (push (cons (1+ length) destination) next-remaining))))
    (thread-last (map-values seen)
      (cl-count-if (lambda (x) (>= x 1000))))))

(defun day20-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((graph               (make-hash-table :test #'equal))
         (max-lisp-eval-depth 10000)
         (max-specpdl-size    30000))
    (regex-to-graph input-file graph (make-hash-table :test #'equal))
    (count-over-1000 graph)))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day20-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day20-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day20-part-1 input-1))
      (message "Part 2: %s\n" (day20-part-2 input-2)))))

(provide 'day20)
;;; day20 ends here
