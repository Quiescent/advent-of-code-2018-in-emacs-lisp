;;; day24 --- My solution to day24 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day24

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

;; ==========TIE BREAKS==========
;; 
;; 1. In decreasing order of effective power, groups choose their
;; targets; in a tie, the group with the higher initiative chooses
;; first.
;;
;; 2. If an attacking group is considering two defending groups to
;; which it would deal equal damage, it chooses to target the
;; defending group with the largest effective power; if there is still
;; a tie, it chooses the defending group with the highest initiative.
;;
;;

;; ==========Interesting Rules==========
;;
;; 1. If it cannot deal any defending groups damage, it does not
;; choose a target.
;;
;; 2. Defending groups can only be chosen as a target by one attacking
;; group.
;;
;; 3. Groups attack in decreasing order of initiative.
;;
;; 4. If the defending group is immune to the attacking group's attack
;; type, the defending group instead takes no damage.
;;
;; 5. If the defending group is weak to the attacking group's attack
;; type, the defending group instead takes double damage
;;
;; 6. The defending group only loses whole units from damage.
;;
;; 7. Any remaining damage to a unit that does not immediately kill it
;; is ignored.

;; ==========Assumptions==========
;;
;; 1. Armies have distinct initiatives.
;;
;; 2. No number statistics are negative.

(defun parse-armies (input-file)
  "Parse the INPUT-FILE into a cons of the immune system and infection army.

An army is:
((INITIATIVE
  DAMAGE
  DAMAGE-TYPE
  UNIT-COUNT
  HIT-POINTS
  (IMMUNITY...)
  (WEAKNESS...))...))"
  (let* ((without-immune-heading (substring input-file (1+ (cl-position ?\n input-file))))
         (army-split             (split-string without-immune-heading "\nInfection:\n" t " ")))
    (cons (parse-army (car  army-split))
          (parse-army (cadr army-split)))))

(require 'subr-x)

(defun parse-damage-type (str)
  "Parse the damage type from STR."
  (progn
    (string-match "does [0-9]+ \\(.*\\) damage" str)
    (intern (upcase (match-string 1 str)))))

(defun parse-subgroup (sub-group)
  "Parse a SUB-GROUP into symbols representing the immunities and weaknesses."
  (thread-last (split-string (substring sub-group (+ 3 (cl-search "to " sub-group)))
                             "," t " ")
    (cl-mapcar #'upcase)
    (cl-mapcar #'intern)))

(defun parse-immunities-and-weaknesses (str)
  "Parse a cons of immunities and weaknesses from STR."
  (let* ((modifier-section (substring str
                                      (string-match "(.*)" str)
                                      (cl-position ?\) str)))
         (sub-groups       (split-string modifier-section ";" t " "))
         (immune-first     (cl-search "immune" (car sub-groups)))
         (has-second-group (eq (length sub-groups) 2))
         (first-group      (parse-subgroup (car sub-groups)))
         (second-group     (when has-second-group (parse-subgroup (cadr sub-groups)))))
    (if immune-first
        (cons first-group  second-group)
        (cons second-group first-group))))

(defun parse-army (army-str)
  "Parse ARMY-STR into the army structure.

See `parse-armies' for structure."
  (cl-mapcar (lambda (line) (let* ((stats       (thread-last (split-string line "[^0-9]" t " ")
                                                  (cl-mapcar #'string-to-number)))
                                   (initiative  (nth 3 stats))
                                   (damage      (nth 2 stats))
                                   (damage-type (parse-damage-type line))
                                   (unit-count  (nth 0 stats))
                                   (hit-points  (nth 1 stats))
                                   (modifiers   (when (cl-search "to " line)
                                                  (parse-immunities-and-weaknesses line)))
                                   (immunities  (and modifiers (car modifiers)))
                                   (weaknesses  (and modifiers (cdr modifiers))))
                              (list initiative
                                    damage
                                    damage-type
                                    unit-count
                                    hit-points
                                    immunities
                                    weaknesses)))
             (split-string army-str "\n" t " ")))

(defun effective-power (tagged-army)
  "Produce the effective power of TAGGED-ARMY."
  (* (nth 2 tagged-army)
     (nth 4 tagged-army)))

(require 'map)

(defun damage-dealt (damage unit-count target-is-immune target-is-weak)
  "Produce the DAMAGE dealt by UNIT-COUNT units.

If the TARGET-IS-IMMUNE then deal no damage.

If the TARGET-IS-WEAK then deal double damage."
  (* unit-count
     (if target-is-immune
         0
         (if target-is-weak
             (* 2 damage)
             damage))))

(defun find-target (other-armies taken enemy-tag army)
  "Find a target in OTHER-ARMIES of tag ENEMY-TAG, which aren't TAKEN, for ARMY."
  (pcase army
    (`(,_ ,damage ,damage-type ,unit-count ,_ ,_ ,_)
      (cl-loop for (others-initiative
                          others-damage
                          _
                          others-unit-count
                          _
                          others-immunities
                          others-weaknesses)
           being the elements of other-armies
         using (index i)
         with best-others-effective-power = nil
         with best-initiative             = nil
         with best-index                  = nil
         with best-damage-dealt           = nil
         for  other-is-immune-to-us       = (memq damage-type others-immunities)
         for  other-is-weak-to-us         = (memq damage-type others-weaknesses)
         for  others-effective-power      = (* others-unit-count others-damage)
         for  our-damage-dealt            = (damage-dealt damage
                                                   unit-count
                                                   other-is-immune-to-us
                                                   other-is-weak-to-us)
         when (and (not (map-elt taken (cons enemy-tag i)))
                   (or (null best-index)
                       (or (> our-damage-dealt
                              best-damage-dealt)
                           (and (eq our-damage-dealt
                                    best-damage-dealt)
                                (> others-effective-power
                                   best-others-effective-power))
                           (and (eq our-damage-dealt
                                    best-damage-dealt)
                                (eq others-effective-power
                                    best-others-effective-power)
                                (> others-initiative
                                   best-initiative)))))
           do (progn
                (setq best-others-effective-power others-effective-power
                      best-initiative             others-initiative
                      best-index                  i
                      best-damage-dealt           our-damage-dealt))
         finally return (progn
                          (setf (map-elt taken (cons enemy-tag best-index)) t)
                          best-index)))))

(defun tick (immune-system infection)
  "Advance IMMUNE-SYSTEM and INFECTION by one unit of targetting and attack."
  (let* ((tagged-armies (append (cl-mapcar (apply-partially #'cons 'IMMUNE)
                                           (cl-subseq immune-system 0))
                                (cl-mapcar (apply-partially #'cons 'INFECTION)
                                           (cl-subseq infection     0))))
         (sorted-armies (thread-first (cl-sort tagged-armies #'> :key #'cadr)
                          (cl-stable-sort #'> :key #'effective-power)))
         (taken         (make-hash-table :test #'equal))
         targets)
    (cl-loop for army in sorted-armies
       for type         = (car army)
       for other-armies = (if (eq type 'IMMUNE) infection  immune-system)
       for other-tag    = (if (eq type 'IMMUNE) 'INFECTION 'IMMUNE)
       do (push (find-target other-armies taken other-tag (cdr army)) targets))
    (setq targets (nreverse targets))
    (message "Immunes: %s, Infections: %s, Targets: %s" (length immune-system) (length infection) targets)
    (let* ((tagged-armies-with-targets (cl-mapcar #'cons targets sorted-armies))
           (sorted-by-initiative       (cl-sort tagged-armies-with-targets
                                                #'>
                                                :key #'caddr)))
      (cl-loop for (target
                     type
                     _
                     damage
                     damage-type
                     unit-count
                     _
                     _
                     _)
         in sorted-by-initiative
         for other-armies = (if (eq type 'IMMUNE) infection immune-system)
         when target
           do (let* ((other-army            (nth target other-armies))
                     (others-unit-count     (nth 3 other-army))
                     (others-hit-points     (nth 4 other-army))
                     (others-immunities     (nth 5 other-army))
                     (others-weaknesses     (nth 6 other-army))
                     (other-is-immune-to-us (memq damage-type others-immunities))
                     (other-is-weak-to-us   (memq damage-type others-weaknesses))
                     (our-damage-this-round (damage-dealt damage
                                                          unit-count
                                                          other-is-immune-to-us
                                                          other-is-weak-to-us))
                     (units-killed          (/ our-damage-this-round others-hit-points))
                     (new-others-unit-count (- others-unit-count units-killed)))
                (setcar (nthcdr 3 other-army) new-others-unit-count)
                (when (<= new-others-unit-count 0)
                  (if (eq type 'IMMUNE)
                      (setcar (nthcdr target infection)     nil)
                      (setcar (nthcdr target immune-system) nil)))))
      (cons (delq nil immune-system)
            (delq nil infection)))))

(defun day24-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((armies        (parse-armies input-file))
         (immune-system (car armies))
         (infection     (cdr armies)))
    (cl-loop while (and (> (length immune-system) 0)
                        (> (length infection)     0))
       for (next-immune-system . next-infection) = (tick immune-system infection)
       do (setq immune-system next-immune-system
                infection     next-infection))
    (thread-last (if (> (length immune-system) 0) immune-system infection)
      (cl-mapcar #'cadddr)
      (apply #'+))))

;; 30631 -- too low

(let* ((test-input    "Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")
       (test-computed (day24-part-1 test-input))
       (test-ans      5216))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day24-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

(let* ((test-input    "")
       (test-computed (day24-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day24-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day24-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day24-part-1 input-1))
      (message "Part 2: %s\n" (day24-part-2 input-2)))))

(provide 'day24)
;;; day24 ends here
