;;;;;;;;;;;;;;;;;;;;
;;; FA structure ;;;
;;;;;;;;;;;;;;;;;;;;

;; Make an DFA
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 0 q0))
           'q0
           '(q1)))

(defparameter *dfa-1*
  (make-fa '((q0 0 q1)
             (q1 1 q0))
           'q0
           '(q1)))

;; Make an NFA
(defparameter *nfa-0*
  (make-fa '((q0 0 q0)
             (q0 :epsilon q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))
(defparameter *dfa-3*
  (make-fa '((q0 0 q1)
	     (q0 1 q3)
	     (q1 0 q4)
	     (q3 0 q4))
	   'q0
	   '(q4)))(defparameter *dfa-1*
  (make-fa '((q0 0 q1)
             (q1 1 q0))
           'q0
           '(q1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0: DFA Simulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (dfa-minimize *dfa-3*))
;;(print (dfa-simulate *dfa-1* '(0 1 0)))
;; => t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1: NFA Subset Construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (e-closure *nfa-0* '(q0) nil))
(print "(q0 q1 q2)")

(print (move-e-closure *nfa-0* '(q0) 1))
(print "(q1 q2)")

(nfa-simulate *nfa-0* '(0 1))
;; => t

(nfa->dfa *nfa-0*)
;; => the equivalent dfa


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2: Regular Expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(simplify-regex '(:? (:union 0 1)) '(0 1))
;; => (:union :epsilon (:union 0 1))

(simplify-regex '(:+ :.) '(0 1))
;; => (:concatenation (:union 0 1) (:kleene-closure (:union 0 1)))

(regex->nfa '(:concatenation (:union 0 1) (:kleene-closure (:union 0 1))))
;; => the equivalent nfa


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3: Regular Decision and Closure Properties ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fa-empty *dfa-0*)
;; => nil

(dfa-minimize *dfa-0*)
;; => the equivalent minimum-state dfa

(dfa-intersection *dfa-0* *dfa-1*)
;; => fa for the intersection of *dfa-0* and *dfa-1*

(dfa-equivalent *dfa-0* *dfa-1*)
;; => nil
