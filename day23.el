;;; day23 --- My solution to day23 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day23

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(require 'subr-x)

(defun parse-bots (input-file)
  "Parse the bots from INPUT-FILE."
  (thread-last (split-string input-file "\n" t " ")
    (cl-mapcar (lambda (line) (thread-last (split-string line "[^0-9-]" t " ")
                                (cl-mapcar #'string-to-number))))))

(defun manhattan-distance-within (bot-1 bot-2)
  "Produce t if BOT-1's signal extends to include BOT-2."
  (<= (+ (abs (- (car bot-1) (car bot-2)))
         (abs (- (cadr bot-1) (cadr bot-2)))
         (abs (- (caddr bot-1) (caddr bot-2))))
      (cadddr bot-1)))

(require 'map)
(require 'seq)

(defun day23-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((bots           (parse-bots input-file))
         (largest-radius (thread-first (seq-subseq bots 0)
                           (cl-sort #'> :key #'cadddr)
                           (car))))
    (cl-loop for bot in bots
       count (manhattan-distance-within largest-radius bot))))

(let* ((test-input    "pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1")
       (test-computed (day23-part-1 test-input))
       (test-ans      7))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; 932: incorrect

;; # PART 2:

(defun spheres-intersect-or-contain (sphere-1 sphere-2)
  "Produce t if SPHERE-1 intersects with SPHERE-2."
  (let* ((distance-between-centres (+ (abs (- (car sphere-1) (car sphere-2)))
                                      (abs (- (cadr sphere-1) (cadr sphere-2)))
                                      (abs (- (caddr sphere-1) (caddr sphere-2))))))
    (<= distance-between-centres
        (+ (cadddr sphere-1)
           (cadddr sphere-2)))))

(defun count-intersection-or-contains (spheres sphere)
  "Produce the number of SPHERES which SPHERE interests with."
  (cl-count-if (apply-partially #'spheres-intersect-or-contain sphere) spheres))

(defun most-intersections-or-contains (spheres)
  "Produce the sphered which intersects with the most in SPHERES.

Also produce the count of spheres which it intersected with."
  (cl-loop for sphere in (cdr spheres)
     with best-count  = (count-intersection-or-contains spheres (car spheres))
     with best-sphere = (car spheres)
     for  next-count  = (count-intersection-or-contains spheres sphere)
     when (> next-count best-count)
       do (setq best-count  next-count
                best-sphere sphere)
     finally return (cons best-count best-sphere)))

(defun manhattan-distance (point-1 point-2)
  "Produce the manhattan distance between POINT-1 and POINT-2."
  (+ (abs (- (car   point-1) (car   point-2)))
     (abs (- (cadr  point-1) (cadr  point-2)))
     (abs (- (caddr point-1) (caddr point-2)))))

(defun distance-to-origin (point)
  "Produce the distance to the origin from POINT."
  (+ (abs (car   point))
     (abs (cadr  point))
     (abs (caddr point))))

(defun all-intersecting-or-containing-spheres (spheres sphere)
  "Produce the SPHERES which intersect with SPHERE."
  (cl-remove-if-not (apply-partially #'spheres-intersect-or-contain sphere) spheres))

(defun completely-contains (sphere-1 sphere-2 distance-between-centres)
  "Produce t if SPHERE-1 completely contains SPHERE-2.

DISTANCE-BETWEEN-CENTRES is the distance between the two
centres."
  (< (+ distance-between-centres
        (cadddr sphere-2))
     (cadddr sphere-1)))

(defun spheres-intersect (sphere-1 sphere-2)
  "Produce t if SPHERE-1 intersects with SPHERE-2."
  (let* ((distance-between-centres (+ (abs (- (car sphere-1) (car sphere-2)))
                                      (abs (- (cadr sphere-1) (cadr sphere-2)))
                                      (abs (- (caddr sphere-1) (caddr sphere-2))))))
    (and (<= distance-between-centres
             (+ (cadddr sphere-1)
                (cadddr sphere-2)))
         (not (completely-contains sphere-1 sphere-2 distance-between-centres))
         (not (completely-contains sphere-2 sphere-1 distance-between-centres)))))

(defun all-intersecting-spheres (sphere spheres)
  "Produce all the spheres which interesect with SPHERE in SPHERES."
  (let* ((possibly-intersecting (cl-loop for other-sphere in spheres
                                   when (spheres-intersect other-sphere sphere)
                                     collect other-sphere)))
    (cl-loop for sphere in possibly-intersecting
       when (cl-every (lambda (other-sphere)
                        (spheres-intersect sphere other-sphere))
               possibly-intersecting)
         collect sphere)))

(defun largest-intersecting-set (spheres)
  "Produce the largest set of SPHERES which intersect."
  (cl-loop for sphere in (cdr spheres)
     with count       = 0
     do (message "Count: %s" count)
     do (cl-incf count)
     with best        = (all-intersecting-spheres (car spheres) spheres)
     with best-length = (length best)
     for  next        = (all-intersecting-spheres sphere        spheres)
     for  next-length = (length next)
     when (> next-length best-length)
       do (setq best        next
                best-length next-length)
     finally return best))

(defun move-closer (x)
  "Get X closer to the origin."
  (if (< 0 x)
      (1+ x)
      (if (eq 0 x)
          x
          (1- x))))

(defun one-closer-to-zero (x)
  "Produce the X which is one closer to zero."
  (if (< x 0)
      (1+ x)
      (if (eq 0 x)
          x
          (1- x))))

(defun closer-heads (head)
  "Produe the points which are closer to the origin from HEAD."
  (pcase head
    (`(,x ,y ,z)
      (list (list (one-closer-to-zero x) y                      z)
            (list (one-closer-to-zero x) (one-closer-to-zero y) z)
            (list (one-closer-to-zero x) (one-closer-to-zero y) (one-closer-to-zero z))
            (list x                      (one-closer-to-zero y) z)
            (list x                      (one-closer-to-zero y) (one-closer-to-zero z))
            (list x                      y                      (one-closer-to-zero z))
            (list (one-closer-to-zero z) y                      (one-closer-to-zero z))))))

(defun expand-search (head seen)
  "Produce the new search points from HEAD.

Points which have been SEEN are not added."
  (thread-last (closer-heads head)
    (cl-delete-if (lambda (new-head)
                    (not (when (null (map-elt seen new-head))
                           (setf (map-elt seen new-head) t)))))))

(defun average (xs)
  "Produce the average of XS."
  (/ (apply #'+ xs)
     (length xs)))

(defun average-point (spheres)
  "Produce the average of the centres of SPHERES."
  (list (average (cl-mapcar #'car   spheres))
        (average (cl-mapcar #'cadr  spheres))
        (average (cl-mapcar #'caddr spheres))))

(defun closest-to (point other-point)
  "Produce the distance from POINT to OTHER-POINT."
  (manhattan-distance point other-point))

(defun sphere-contains (sphere point)
  "Produce t if SPHERE contains POINT."
  (<= (+ (abs (- (car sphere) (car point)))
         (abs (- (cadr sphere) (cadr point)))
         (abs (- (caddr sphere) (caddr point))))
      (cadddr sphere)))

(defun count-containing-spheres (spheres point)
  "Produce the number of SPHERES which contain POINT."
  (cl-count-if (lambda (sphere) (sphere-contains sphere point)) spheres))

;; Approach:
;; 
;; Find the intersecting set and hard code it.
;; 
;; Find the start and the smallest and hard code them.
;; 
;; Try points which I think are reasonable with the random search
;; around.
;;
;; Basically do it semi-by hand.

(defun day23-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((bots                       (parse-bots input-file))
         (best-intersection          (most-intersections-or-contains bots))
         (containing-sphere          (cdr best-intersection))
         (intersecting-or-containing (all-intersecting-or-containing-spheres
                                      bots
                                      containing-sphere))
         (intersecting               (largest-intersecting-set intersecting-or-containing))
         (smallest-sphere            (cl-sort (cl-subseq intersecting 0) #'< :key #'cadddr))
         (starting-point             (average-point intersecting))
         (priority-function          (apply-partially #'closest-to starting-point)))
    (message "Intersecting: %s" intersecting)
    (message "Smallest sphere: %s" smallest-sphere)
    (message "Starting point: %s" starting-point)
    (cl-loop
       with best-count = (count-containing-spheres intersecting starting-point)
       with best-heads = nil
       with seen       = (make-hash-table :test #'equal)
       with heads      = (list starting-point)
       for head = (pop heads)
       for next-count = (count-containing-spheres intersecting head)
       if (> next-count best-count)
         do (progn
              (message "Found better!")
              (setq best-count next-count
                    best-heads (list head)))
       else when (eq next-count best-count)
              do (progn
                   (push head best-heads)
                   (let* ((next-heads (expand-search head seen)))
                     (when next-heads
                       (setq heads (cl-merge 'list
                                             heads
                                             (cl-sort next-heads
                                                      #'<
                                                      :key priority-function)
                                             #'<
                                             :key priority-function)))))
       do (message "Head count: %s" (length heads))
       when (eq (length heads) 0)
         do (message "Best: %s" (car best-heads))
       while (> (length heads) 0)
       finally return (thread-first (cl-mapcar (lambda (head) (distance-to-origin head))                                      best-heads)
                        (cl-sort #'<)
                        (car)))))

;; Too high: 228293349
;; Too low:  71368671
;; Too low:  72139666

(let* ((test-input    "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5")
       (test-computed (day23-part-2 test-input))
       (test-ans      36))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day23-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day23-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day23-part-1 input-1))
      (message "Part 2: %s\n" (day23-part-2 input-2)))))

(provide 'day23)
;;; day23 ends here
