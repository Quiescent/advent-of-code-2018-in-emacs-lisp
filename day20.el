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

(defun regex-to-graph (regex pos graph x y start-x start-y seen)
  "Parse REGEX starting from POS and create GRAPH.

Start at X, Y in the GRAPH.

START-X and START-Y are the coords where this subgroup started.

SEEN documents the pos-coord pairs which we've already seen to
prevent duplicated searches."
  (cl-loop
     while (< pos (length regex))
     with next-x  = 0
     with next-y  = 0
     for  char    = (aref regex pos)
     when (map-elt seen (cons pos (cons x y)))
       do (progn
            (message "Seen: %s" (cons pos (cons x y)))
            (cl-return))
     do (setf (map-elt seen (cons pos (cons x y))) t)
     do (message "%s: %s" pos (char-to-string char))
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
                       y next-y)
                 (cl-incf pos)))
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
                       y next-y)
                 (cl-incf pos)))
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
                       y next-y)
                 (cl-incf pos)))
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
                       y next-y)
                 (cl-incf pos)))
          (?\( (let* ((pos-and-coords (regex-to-graph regex (1+ pos) graph x y x y seen))
                      (end-pos        (car pos-and-coords))
                      (coords         (cdr pos-and-coords))
                      next-pos
                      next-coords)
                 ;; For each coord, continue parsing.
                 (when (null end-pos)
                   (cl-return nil))
                 (cl-return
                   (cl-loop for coord in coords
                      for  (x . y)     = coord
                      do (progn
                           (let* ((end-pos-and-coords (regex-to-graph regex
                                                                      end-pos
                                                                      graph
                                                                      x
                                                                      y
                                                                      start-x
                                                                      start-y
                                                                      seen)))
                             (setq next-pos (car end-pos-and-coords))
                             (setq next-coords (cl-union next-coords
                                                         (cdr end-pos-and-coords)))))
                      finally return (cons next-pos next-coords)))))
          (?\) (cl-return (cons (1+ pos) (list (cons x y)))))
          (?|  (progn
                 ;; Step 1: We start from the starting coord for this
                 ;; subgroup and continue with the alternate route.
                 (let* ((end-pos-and-coords (regex-to-graph regex
                                                            (1+ pos)
                                                            graph
                                                            start-x
                                                            start-y
                                                            start-x
                                                            start-y
                                                            seen))
                        (pos-end  (car end-pos-and-coords))
                        (coords   (cdr end-pos-and-coords)))
                   ;; Step 2: We know where the subgroup ended.  Now
                   ;; we return the position at the end of the group
                   ;; along with the coords which the right parse
                   ;; generated with the current coordinate added.
                   (when (null pos-end) (cl-return))
                   (cl-return (cons pos-end
                                    (cons (cons x y) coords))))))
          (?$  (progn
                 (cl-incf pos)
                 (message "Found end!")))
          (?^  (cl-incf pos))
          (`_  (cl-incf pos)))
     finally return (cons pos (list (cons x y)))))

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
    (regex-to-graph input-file 0 graph 0 0 0 0 (make-hash-table :test #'equal))
    (longest-path graph)))

;; (let* ((test-input    "^WNE$")
;;        (test-computed (day20-part-1 test-input))
;;        (test-ans      3))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "^ENWWW(NEEE|SSE(EE|N))$")
;;        (test-computed (day20-part-1 test-input))
;;        (test-ans      10))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
;;        (test-computed (day20-part-1 test-input))
;;        (test-ans      18))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
;;        (test-computed (day20-part-1 test-input))
;;        (test-ans      23))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
       (test-computed (day20-part-1 test-input))
       (test-ans      31))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Too low: 941

;; # PART 2:

(defun day20-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

(let* ((test-input    "")
       (test-computed (day20-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

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
