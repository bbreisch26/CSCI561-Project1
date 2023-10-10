;;;;;;;;;;;;;;;;;;;
;136;0c;;;; Utilities ;;;
;;;;;;;;;;;;;;;;;;;

(defun hash-table-keys (hash-table)
  "Return the hash table's keys as a list."
  (let ((result nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key result))
             hash-table)
    result))

(defun make-symbol-hash-table ()
  "Convenience function to make a hash table for FA symbols."
  (make-hash-table :test #'equal))

(defun fold-left (function initial-value list)
  "Convenience function for fold-left with an initial value."
  (reduce function list :initial-value initial-value))

(defun TODO (thing)
  "Placeholder for code to implement."
  (error "Unimplemented: ~A" thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STARTER DEFINITIONS FOR FINITE AUTOMATA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A structure type for finite automata
(defstruct finite-automaton
  "A Finite Automaton."
  states ; state set as a list
  alphabet ; input alphabet as a list
  edges ; list of edges: (state-0 input-symbol state-1)
  delta ; transition function : state-0 * input-symbol -> state-1
  start ; start state
  accept) ; accept set as a list

(defun make-fa (edges start accept)
  "Convenience constructor for finite automata"
  (flet ((add-state (hash state)
                    (setf (gethash state hash) t)
                    hash)
         (add-edge-states (hash edge)
                          (destructuring-bind (state-0 input-symbol state-1) edge
                            (declare (ignore input-symbol))
                            (setf (gethash state-0 hash) t
                              (gethash state-1 hash) t))
                          hash)
         (add-edge-input-symbol (hash edge)
                                (destructuring-bind (state-0 input-symbol state-1) edge
                                  (declare (ignore state-0 state-1))
                                  (setf (gethash input-symbol hash) t))
                                hash)
         (add-edge-transition (hash edge)
                              (destructuring-bind (state-0 input-symbol state-1) edge
                                (push state-1 (gethash (cons state-0 input-symbol) hash)))
                              hash))
    (let ((state-hash (fold-left #'add-edge-states
                                 (fold-left #'add-state
                                            (add-state (make-symbol-hash-table)
                                                       start)
                                            accept)
                                 edges))
          (alphabet-hash (fold-left #'add-edge-input-symbol
                                    (make-symbol-hash-table)
                                    edges))
          (edge-hash (fold-left #'add-edge-transition

                                (make-symbol-hash-table)
                                edges)))
      (make-finite-automaton
        :states (hash-table-keys state-hash)
        :alphabet (hash-table-keys alphabet-hash)
        :edges edges
        :delta (lambda (state-0 input-symbol)
                 (gethash (cons state-0 input-symbol) edge-hash))
        :start start
        :accept accept))))


;;; Higher-order conveience functions for Finite Automata ;;;

(defun map-fa-states (function fa)
  "Map FUNCTION over the FA's states."
  (map 'list function (finite-automaton-states fa)))

(defun map-fa-accept (function fa)
  "Map FUNCTION over the FA's accept states."
  (map 'list function (finite-automaton-accept fa)))

(defun fold-fa-states (function initial-value fa)
  "Fold FUNCTION over the FA's states."
  (fold-left function initial-value (finite-automaton-states fa)))

(defun fold-fa-alphabet (function initial-value fa)
  "Fold FUNCTION over the FA's alphabet."
  (fold-left function initial-value (finite-automaton-alphabet fa)))

(defun map-fa-edges (function fa)
  "Map FUNCTION over the FA's edges."
  (map 'list
      (lambda (edge)
        (destructuring-bind (state-0 input-symbol state-1) edge
          (funcall function state-0 input-symbol state-1)))
    (finite-automaton-edges fa)))

;;; Graphviz Output ;;;

(defun fa-dot (fa place)
  "Output a Graphviz dot file of FA."
  (let ((hash (make-symbol-hash-table)))
    ;; number the states
    (fold-fa-states (lambda (i state)
                      (setf (gethash state hash) i)
                      (1+ i))
                    0 fa)
    ;; Output the Graphviz dot file
    (labels ((state-number (state)
                           (gethash state hash))
             (dot-symbol (thing) ; Pretty-print Greek letters
                         (case thing
                           (:alpha "&alpha;")
                           (:beta "&beta;")
                           (:gamma "&gamma;")
                           (:delta "&delta;")
                           (:epsilon "&epsilon;")
                           (:zeta "&zeta;")
                           (:eta "&eta;")
                           (:theta "&theta;")
                           (:iota "&iota;")
                           (:kappa "&kappa;")
                           (:lambda "&lambda;")
                           (:mu "&mu;")
                           (:nu "&nu;")
                           (:xi "&xi;")
                           (:omicron "&omicron;")
                           (:pi "&pi;")
                           (:rho "&rho;")
                           (:sigma "&sigma;")
                           (:tau "&tau;")
                           (:upsilon "&upsilon;")
                           (:phi "&phi;")
                           (:chi "&chi;")
                           (:omega "&omega;")
                           (t thing)))
             (helper (stream)
                     ;; output
                     (format stream "~&digraph { ~%")
                     ;; state labels
                     (format stream "~:{~&  ~A[label=\"~A\"];~}"
                       (map-fa-states (lambda (state)
                                        (list (state-number state)
                                              state))
                                      fa))
                     ;; start state
                     (format stream "~&  start[shape=none];")
                     (format stream "~&  start -> ~A;"
                       (state-number (finite-automaton-start fa)))
                     ;; accept state
                     (format stream "~:{~&  ~A [ shape=~A ];~}"
                       (map-fa-accept (lambda (q)
                                        (list (state-number q) "doublecircle"))
                                      fa))
                     ;; edges
                     (format stream "~:{~&  ~A -> ~A [fontsize=~D,label=\"~A\"];~%~}"
                       (map-fa-edges (lambda (state-0 input-symbol state-1)
                                       (list (state-number state-0)
                                             (state-number state-1)
                                             12 (dot-symbol input-symbol)))
                                     fa))
                     ;; end
                     (format stream "~&}~%")))
      (cond
       ((streamp place)
         (helper place))
       ((eq place t)
         (helper *standard-output*))
       ((null place)
         (with-output-to-string (s)
           (helper s)))
       ((or (stringp place)
            (pathnamep place))
         (with-open-file (stream place
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
           (helper stream)))
       (t (error "Unrecognized output type: ~A" place))))))


#+sbcl
(defun fa-pdf (fa filename)
  "Create a PDF of FA."
  (with-input-from-string (text (fa-dot fa nil))
    (sb-ext:run-program "dot" (list "-Tpdf")
                        :search t
                        :input text
                        :output filename
                        :if-output-exists :supersede)))


;;; Finite Automata Helper Functions ;;;

(defun fa-transition (fa state-0 input-symbol)
  "Return the list of successors of STATE-0 on INPUT-SYMBOL."
  (funcall (finite-automaton-delta fa)
    state-0 input-symbol))

(defun dfa-transition (fa state-0 input-symbol)
  "Return the successor of STATE-0 on INPUT-SYMBOL."
  (assert (not (eq input-symbol :epsilon)))
  (let ((successors (fa-transition fa state-0 input-symbol)))
    ;; DFA cannot have multiple successors, or it would be
    ;; nondeterministic
    (assert (or (null successors)
                (null (cdr successors))))
    (car successors)))

(defun newstate (&optional (arg "Q-"))
  "Construct a unique state for a finite automaton."
  (gensym arg))

(defun dfa-p (fa)
  "Is FA is deterministic?"
  (labels ((rec (hash edges)
                ;; Recurse over the edges and check for nondeterministic
                ;; transitions.
                ;; hash : (CONS state-0  input-symbol) -> (or T NIL)
                (if edges
                    (destructuring-bind ((state-0 input-symbol state-1) &rest edges) edges
                      (declare (ignore state-1))
                      (let ((key (list state-0 input-symbol)))
                        (unless (or (eq input-symbol :epsilon)
                                    (gethash key hash))
                          (setf (gethash key hash) t)
                          (rec hash edges))))
                    t)))
    (and (finite-automaton-p fa)
         (rec (make-symbol-hash-table)
              (finite-automaton-edges fa)))))

(defun dfa-add-reject (dfa &optional (alphabet (finite-automaton-alphabet dfa)))
  "Add a reject state to DFA."
  (assert (dfa-p dfa))
  ;; Index non-dead-state edges
  ;; hash : (CONS state-0 input-symbol) -> (OR T NIL)
  (let ((dead-state (newstate "dead")))
    ;; Create edges to a dead-state
    (let ((edges (fold-left (lambda (edges input-symbol) ; fold over alphabet
                              (fold-left (lambda (edges state) ; fold over states
                                           (if (fa-transition dfa state input-symbol)
                                               edges
                                               (cons (list state input-symbol dead-state)
                                                     edges)))
                                         ;; dead-state self transition
                                         (cons (list dead-state input-symbol dead-state)
                                               edges)
                                         (finite-automaton-states dfa)))
                            (finite-automaton-edges dfa)
                            alphabet)))
      ;; Result
      (make-fa edges
               (finite-automaton-start dfa)
               (finite-automaton-accept dfa)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMPLETE THE FUNCTIONS BELOW ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0: DFA Simulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finite Automata Lecture: Algorithm 1
(defun dfa-simulate (dfa sequence)
  "True if DFA accepts SEQUENCE."
  (assert (dfa-p dfa))
  (labels ((edelta (state list)
                   (if (null list)
                       state
                       (edelta (dfa-transition dfa state (car list)) (cdr list)))))
    (let ((final-state (edelta (finite-automaton-start dfa)
                               (coerce sequence 'list)))) ; Coerce to list for simplicity
      (if (find final-state (finite-automaton-accept dfa) :test #'equal)
          t
          nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1: NFA Subset Construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Subset Construction Lecture: Algorithm 1
(defun e-closure (nfa s c)
  (labels ((visit (c q)
                  (if (member q c)
                      c
                      ;;Need delta(Q,eps), {q} union C
                      (e-closure nfa (fa-transition nfa q :epsilon) (union (list q) c)))))
    (fold-left #'visit c s)))


;; Subset Construction Lecture: Algorithm 2
(defun move-e-closure (nfa s a)
  (labels ((visit (c q)
                  (e-closure nfa (fa-transition nfa q a) c)))
    (fold-left #'visit nil (e-closure nfa s nil))))

;; Subset Construction Lecture: Algorithm 4
(defun nfa-simulate (nfa sequence)
  "True if NFA accepts SEQUENCE."
  (labels ((edelta (subset list)
                   (if (null list)
                       subset
                       (let ((u (move-e-closure nfa subset (car list))))
                         (edelta u (cdr list))))))
    (let* ((q0 (finite-automaton-start nfa))
           (f (finite-automaton-accept nfa))
           (u (e-closure nfa (list q0) nil))
           (list (coerce sequence 'list))
           (final-subset (edelta u list)))
      (if (intersection final-subset f
                        :test #'equal)
          t
          nil))))

(defun state-predicate-atom (a b)
  "Predicate function to compare atomic states."
  (etypecase a
    ((or symbol string)
     (etypecase b
       ((or symbol string)
        (string-lessp a b))
       (number nil)))
    (number
     (etypecase b
       ((or symbol string)
        t)
       (number (<= a b))))))

(defun state-predicate (a b)
  "Predicate function to compare FA states."
  (etypecase a
    (atom (etypecase b
            (atom (state-predicate-atom a b))
            (list t)))
    (cons (etypecase b
            (atom nil)
            (cons (if (equal (car a) (car b))
                      (state-predicate (cdr a)
                                       (cdr b))
                      (state-predicate (car a)
                                       (car b))))))))

(defun nfa->dfa (nfa)
  "Convert a nondeterministic finite automaton to a deterministic finite automaton."
  (let ((visited-subsets (make-symbol-hash-table))
        (alphabet (remove :epsilon (finite-automaton-alphabet nfa))))
    (labels ((sort-subset (u)
                          ;; sort subsets so we can quickly test for previously
                          ;; visited subsets in the hash table
                          (sort u #'state-predicate))
             (visit-symbol (edges starting-subset input-symbol)
                           (let ((new-subset (sort-subset (move-e-closure nfa starting-subset input-symbol))))
                             (if new-subset
                                 (let* ((new-edge `(,starting-subset ,input-symbol ,new-subset))
                                        (updated-edges (sort-subset (cons new-edge edges))))
                                   (visit-subset updated-edges new-subset))
                                 edges)))
             (visit-subset (edges subset)
                           (if (gethash subset visited-subsets)
                               edges
                               (labels ((update-edges (edges-state sigma) (visit-symbol edges-state subset sigma)))
                                 (setf (gethash subset visited-subsets) subset)
                                 (fold-left #'update-edges edges alphabet)))))
      (let* ((starting-subset (e-closure nfa (list (finite-automaton-start nfa)) nil))
             (edges (visit-subset nil starting-subset))
             (old-accept (finite-automaton-accept nfa))
             (new-accept (list)))
        (labels ((push-if-accept (k v)
                                 (declare (ignore k))
                                 (if (not (null (intersection v old-accept)))
                                     (push v new-accept))))
          (maphash #'push-if-accept visited-subsets)
          (make-fa edges starting-subset new-accept))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2: Regular Expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We represent regular expressions as S-expressions, using the
;; following operators:
;;
;; - :union
;; - :concatenation
;; - :kleene-closure
;;
;; The union and concatenation operators are n-ary.  Kleene-closure
;; takes a single argument.
;;
;; We can represent any regular language with a regular expression
;; using only the operators :union, :concatenation, :kleene-closure.
;; However, it is often convenient (and common practice) to define
;; additional operators. The SIMPLIFY-REGEX function will take as
;; input a regular expression with such additional operators and
;; return a simplified expression that contains only the operators
;; :union, :concatenation, ang :kleene-closure.  Specifically, it will
;; simplify the following operators:
;;
;; - :.     -> (:union alphabet...)
;; - (:? X) -> (:union X :epsilon)
;; - (:+ X) -> (:concatenation X (:kleene-closure X))
(defun simplify-regex (regex &optional alphabet)
  "Convert :., :?, :+ to only :union, :concatenation, :kleene-closure"
  (labels ((h (regex)
              (cond
               ((eq regex :.)
                 (assert alphabet)
                 `(:union ,@alphabet))
               ((atom regex)
                 regex)
               (t (destructuring-bind (operator &rest args) regex
                    (case operator
                      (:.
                       (if alphabet
                           `(:union ,@alphabet)
                           '(:union :epsilon)))
                      (:?
                       (if (null args)
                           `(:union ,@alphabet :epsilon)
                           `(:union ,@(mapcar #'h args) :epsilon)))
                      (:+
                       (if (null args)
                           `(:concatenation ,@alphabet (:kleene-closure ,@alphabet))
                           `(:concatenation ,@(mapcar #'h args) (:kleene-closure ,@(mapcar #'h args)))))
                      (t
                       (cons operator (mapcar #'h args)))))))))
    (h regex)))

;; Regular Expression Lecture: Concatenation.
;; Provided in complete form as an example
(defun fa-concatenate (nfa-1 nfa-2)
  "Find the concatenation of NFA-1 and NFA-2."
  (assert (not (intersection (finite-automaton-states nfa-1)
                             (finite-automaton-states nfa-2))))
  (let ((start (newstate))
        (accept (newstate)))
    (make-fa (append (list (list start :epsilon (finite-automaton-start nfa-1)))
               (map 'list (lambda (x)
                            (list x :epsilon (finite-automaton-start nfa-2)))
                 (finite-automaton-accept nfa-1))
               (map 'list (lambda (x)
                            (list x :epsilon accept))
                 (finite-automaton-accept nfa-2))
               (finite-automaton-edges nfa-1)
               (finite-automaton-edges nfa-2))
             start
             (list accept))))

;; Regular Expression Lecture: Kleene-Closure
(defun fa-repeat (nfa)
  "Find the repetition / Kleene-closure of NFA."
  (let ((start (newstate))
        (accept (newstate)))
    ;; Add new e-transition - new start to old nfa start
    ;;               - old accept to new accept
    ;;               - new start to new accept
    ;;               - old accept to old start
    (make-fa (append (list (list start :epsilon (finite-automaton-start nfa)))
               (list (list start :epsilon accept))
               (map 'list (lambda (x) (list x :epsilon start)) (finite-automaton-accept nfa))
               (map 'list (lambda (x) (list x :epsilon accept)) (finite-automaton-accept nfa))
               (finite-automaton-edges nfa))
             start
             (list accept))))


;; Regular Expression Lecture: Union
(defun fa-union (nfa-1 nfa-2)
  "Find the union of NFA-1 and NFA-2."
  (assert (not (intersection (finite-automaton-states nfa-1)
                             (finite-automaton-states nfa-2))))
  (let ((start (newstate))
        (accept (newstate)))
    (make-fa (append (list (list start :epsilon (finite-automaton-start nfa-1)))
               (list (list start :epsilon (finite-automaton-start nfa-2)))
               (map 'list (lambda (x)
                            (list x :epsilon accept))
                 (finite-automaton-accept nfa-1))
               (map 'list (lambda (x)
                            (list x :epsilon accept))
                 (finite-automaton-accept nfa-2))
               (finite-automaton-edges nfa-1)
               (finite-automaton-edges nfa-2))
             start
             (list accept))))


;; McNaughton-Yamada-Thompson Algorithm Lecture: Algorithm 1
;;
;; Convert a regular expression to a nondeterministic finite
;; automaton.
;;
;; The following are examples of possible regular expressions.
;;
;; - (:concatenation a b c)
;; - (:union a b c :epsilon)
;; - (:union)
;; - (:kleene-closure a)
;; - (:concatenation (:union a b) (:kleene-closure c))
(defun regex->nfa (regex)
  "Convert a regular expression to an NFA."
  (labels ((MYT-base (regex)
                     (if (null regex)
                         (make-fa nil (newstate) (list (newstate)))
                         (let ((start (newstate)) (accept (newstate)))
                           (make-fa (list (list start regex accept)) start (list accept)))))
           (MYT-union (regex)
                      (if (null regex)
                          (MYT-base regex)
                          (fold-left (lambda (m r) (fa-union m (regex->nfa r))) (regex->nfa (car regex)) (cdr regex))))
           (MYT-concatenate (regex)
                            (if (null regex)
                                (MYT-base :epsilon)
                                (fold-left (lambda (m r) (fa-concatenate m (regex->nfa r))) (regex->nfa (car regex)) (cdr regex)))))
    (cond
     ((null regex)
       (make-fa nil (newstate) (list (newstate))))
     ((atom regex) ; Base case for empty set, empty string
                  (MYT-base regex))
     ((eq (car regex) :kleene-closure)
       (fa-repeat (regex->nfa (first (rest regex)))))
     ((eq (car regex) :concatenation)
       (MYT-concatenate (cdr regex)))
     ((eq (car regex) :union)
       (MYT-union (cdr regex)))
     ;;Single symbol or empty set
     (t nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3: Regular Decision and Closure Properties ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Used in intersection and equivalent - finds cartesian product of a list of states (useful for finding accept states)
(defun states-cartesian (states-0 states-1)
  (labels ((outer-helper (outer-prod-list state-0)
                         (labels ((inner-helper (prod-list state-1)
                                                (cons (list state-0 state-1) prod-list)))
                           (fold-left #'inner-helper outer-prod-list states-1))))
    (fold-left #'outer-helper (list) states-0)))

(defun add-reject-if-necessary-to-dfa (dfa)
  (let* ((src-trans-all (states-cartesian (finite-automaton-states dfa) (finite-automaton-alphabet dfa)))
         (src-trans-pre (maplist (lambda (x)
                                   (let* ((edge (car x))
                                          (src (nth 0 edge))
                                          (tra (nth 1 edge)))
                                     (list src tra))) (finite-automaton-edges dfa)))
         (rej-trans (set-difference src-trans-all src-trans-pre :test #'equal)))
    (if (null rej-trans)
        dfa
        (let* ((reject-title "this-is-the-added-reject-state")
               (reject-loopback-edges (maplist (lambda (x) (list reject-title (car x) reject-title)) (finite-automaton-alphabet dfa)))

               (reject-edges (union reject-loopback-edges (maplist (lambda (x)
                                                                     (let* ((edge (car x))
                                                                            (src (nth 0 edge))
                                                                            (tra (nth 1 edge)))
                                                                       (list src tra reject-title))) rej-trans)))
               (new-edges (union reject-edges (finite-automaton-edges dfa))))
          (make-fa new-edges (finite-automaton-start dfa) (finite-automaton-accept dfa))))))

; Used in intersection and equivalent - finds cartesian product of edges (useful for constructing DFAS)
(defun dfa-cartesian-edges (dfa-0 dfa-1)
  (labels ((outer-helper (outer-edges dfa-0-edge)
                         ; Deconstruct the edge in dfa-0
                         (let ((edge-0-src (nth 0 dfa-0-edge))
                               (edge-0-dst (nth 2 dfa-0-edge))
                               (edge-0-tra (nth 1 dfa-0-edge)))
                           (labels ((inner-helper (inner-edges dfa-1-edge)
                                                  ; Deconstruct the edge in dfa-1
                                                  (let ((edge-1-src (nth 0 dfa-1-edge))
                                                        (edge-1-dst (nth 2 dfa-1-edge))
                                                        (edge-1-tra (nth 1 dfa-1-edge)))
                                                    ; Check if new edge is valid
                                                    (if (equal edge-0-tra edge-1-tra)
                                                        ; If the edges match, add a new edge to the cartesian
                                                        (let ((new-src (list edge-0-src edge-1-src))
                                                              (new-dst (list edge-0-dst edge-1-dst)))
                                                          (cons (list new-src edge-0-tra new-dst) inner-edges))
                                                        ; If the edges do not match, do not add a new edge to the cartesian
                                                        inner-edges))))
                             (fold-left #'inner-helper outer-edges (finite-automaton-edges dfa-1))))))
    (fold-left #'outer-helper (list) (finite-automaton-edges dfa-0))))

;; Lecture: Decision Properties of Regular Languages, Emptiness
(defun fa-empty (fa)
  "Does FA represent the empty set?"
  ;; Use hash tables to keep track of accept states and visited hashes
  (let ((acceptstates (make-symbol-hash-table))
        (visitedhash (make-symbol-hash-table)))
    ;;sort subsets to find previously visited subsets in hashtable
    (labels ((find_accept (state)
                          (if (car state)
                              ;;state is an accept state
                              (if (gethash (car state) acceptstates)
                                  t
                                  ;;not an accept state and have visited this state?
                                  (if (gethash (car state) visitedhash)
                                      nil
                                      (progn (setf (gethash (car state) visitedhash) t)
                                             (fold-fa-alphabet (lambda (acc symbol) (or acc (find_accept (fa-transition fa (car state) symbol)))) nil fa))))
                              nil)))
      ;;Set initial accept state values in hash table
      (map-fa-accept (lambda (accept) (setf (gethash accept acceptstates) t)) fa)
      (not (find_accept (list (finite-automaton-start fa)))))))

;; Reverse function for any fa
(defun fa-reverse (fa)
  (let ((start (newstate "S-")))
    ;; Add new e-transition - start to old accepts 
    (make-fa (append (map 'list (lambda (x) (list start :epsilon x)) (finite-automaton-accept fa))
               ;;Reverse edge map goes here this might work?
               (map 'list (lambda (x) (reverse x)) (finite-automaton-edges fa)))
             start
             (list (finite-automaton-start fa)))))

;; Lecture: Closure Properties of Regular Languages, State Minimization
(defun dfa-minimize (dfa)
  (nfa->dfa (fa-reverse (nfa->dfa (fa-reverse dfa)))))

;; Lecture: Closure Properties of Regular Languages, Intersection
(defun dfa-intersection (dfa-0 dfa-1)
  (let ((accept (states-cartesian (finite-automaton-accept dfa-0) (finite-automaton-accept dfa-1)))
        (start (list (finite-automaton-start dfa-0) (finite-automaton-start dfa-1)))
        (edges (dfa-cartesian-edges dfa-0 dfa-1)))
    (make-fa edges start accept)))

;; Lecture: Decision Properties of Regular Languages, Equivalence
(defun dfa-equivalent (dfa-0-pre dfa-1-pre)
  (let* ((dfa-0 (add-reject-if-necessary-to-dfa dfa-0-pre))
         (dfa-1 (add-reject-if-necessary-to-dfa dfa-1-pre))
         (dfa-0-accept (finite-automaton-accept dfa-0))
         (dfa-0-no-accept (set-difference (finite-automaton-states dfa-0) dfa-0-accept :test #'equal))
         (dfa-1-accept (finite-automaton-accept dfa-1))
         (dfa-1-no-accept (set-difference (finite-automaton-states dfa-1) dfa-1-accept :test #'equal))
         (accept (union (states-cartesian dfa-0-accept dfa-1-no-accept) (states-cartesian dfa-0-no-accept dfa-1-accept)))
         (start (list (finite-automaton-start dfa-0) (finite-automaton-start dfa-1)))
         (edges (dfa-cartesian-edges dfa-0 dfa-1))
         (new-dfa (make-fa edges start accept)))
    (fa-empty new-dfa)))
