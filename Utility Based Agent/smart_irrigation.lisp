;;;; smart_irrigation.lisp
;;;; Smart Irrigation System — four agent types (Common Lisp)
;;;; Run with SBCL: sbcl --script smart_irrigation.lisp

(defpackage :smart-irrigation
  (:use :cl))

(in-package :smart-irrigation)

;;;; ============================================================================
;;;; ENVIRONMENT
;;;; ============================================================================

(defparameter *evaporation-rate* 0.02)
(defparameter *irrigation-gain* 0.25)
(defparameter *rain-gain* 0.3)

(defstruct env
  (moisture 0.5 :type float)
  (raining nil :type boolean)
  (water-available 100 :type float))

(defun step-environment (env action)
  "Apply action and environment effects. Returns modified env."
  (let* ((m (env-moisture env))
         (m (+ m (if (eq action :irrigate) *irrigation-gain* 0.0)))
         (m (+ m (if (env-raining env) *rain-gain* 0.0)))
         (m (- m *evaporation-rate*))
         (m (min 1.0 (max 0.0 m))))
    (setf (env-moisture env) m)
    env))

;;;; ============================================================================
;;;; AGENT 1: SIMPLE REFLEX AGENT (IF-THEN rules, no memory)
;;;; ============================================================================

(defun simple-reflex-agent (percept)
  "IF-THEN rules: no internal state or memory."
  (let ((moisture (getf percept :moisture))
        (raining (getf percept :raining)))
    (cond
      ((or raining (>= moisture 0.5)) :wait)
      ((< moisture 0.3) :irrigate)
      (t :wait))))

;;;; ============================================================================
;;;; AGENT 2: MODEL-BASED REFLEX AGENT (with memory/internal state)
;;;; ============================================================================

(defun make-model-based-reflex-agent ()
  "Returns a closure that maintains internal state (memory)."
  (let ((state (list :last-rain nil :last-action nil)))
    (lambda (percept)
      (let* ((moisture (getf percept :moisture))
             (raining (getf percept :raining))
             (action
               (cond
                 (raining :wait)
                 ((< moisture 0.35) :irrigate)
                 ((and (getf state :last-action) 
                       (eq (getf state :last-action) :irrigate)
                       (< moisture 0.45)) :irrigate)
                 (t :wait))))
        (setf (getf state :last-rain) raining)
        (setf (getf state :last-action) action)
        action))))

;;;; ============================================================================
;;;; AGENT 3: GOAL-BASED AGENT (plans to achieve a goal)
;;;; ============================================================================

(defun make-goal-based-agent (goal-min goal-max)
  "Agent tries to keep moisture within [goal-min, goal-max]."
  (lambda (percept)
    (let ((moisture (getf percept :moisture))
          (raining (getf percept :raining)))
      (cond
        (raining :wait)
        ((< moisture goal-min) :irrigate)
        ((> moisture goal-max) :wait)
        (t :wait)))))

;;;; ============================================================================
;;;; AGENT 4: UTILITY-BASED AGENT (uses numerical utility function)
;;;; ============================================================================

(defun compute-utility (moisture action)
  "Compute utility: prefers moisture near goal (0.6), penalizes water cost."
  (let* ((goal 0.6)
         (distance-penalty (abs (- moisture goal)))
         (comfort (- 1.0 distance-penalty))
         (water-cost (if (eq action :irrigate) 0.4 0.0)))
    (- comfort water-cost)))

(defun utility-based-agent (percept)
  "Choose action with highest utility."
  (let ((moisture (getf percept :moisture))
        (raining (getf percept :raining)))
    (if raining
        :wait
        (let ((u-irrigate (compute-utility moisture :irrigate))
              (u-wait (compute-utility moisture :wait)))
          (if (> u-irrigate u-wait) :irrigate :wait)))))

;;;; ============================================================================
;;;; SIMULATOR
;;;; ============================================================================

(defun make-percept (env)
  "Create percept from environment state."
  (list :moisture (env-moisture env) :raining (env-raining env)))

(defun simulate-agent (agent agent-name steps initial-moisture rain-prob)
  "Run a single agent simulation."
  (format t "~%=== ~A Agent (~A steps) ===~%" agent-name steps)
  (let ((env (make-env :moisture initial-moisture :raining nil :water-available 100)))
    (dotimes (i steps)
      ;; Simulate random rain event
      (setf (env-raining env) (< (random 1.0) rain-prob))
      
      ;; Get percept and action
      (let* ((percept (make-percept env))
             (action (funcall agent percept)))
        
        ;; Print step info
        (format t "Step ~2D | Moisture: ~5,2F | Raining: ~A | Action: ~A~%"
                (1+ i) (env-moisture env) (env-raining env) action)
        
        ;; Apply action and advance environment
        (step-environment env action)))))

(defun run-simulations ()
  "Run simulations for all four agent types."
  (format t "~%~%========== SMART IRRIGATION SYSTEM ==========~%")
  (format t "Simulating four agent types in irrigation environment~%~%")
  
  ;; Agent 1: Simple Reflex
  (simulate-agent #'simple-reflex-agent "SIMPLE-REFLEX" 12 0.25 0.15)
  
  ;; Agent 2: Model-Based Reflex
  (let ((mb-agent (make-model-based-reflex-agent)))
    (simulate-agent mb-agent "MODEL-BASED-REFLEX" 12 0.25 0.15))
  
  ;; Agent 3: Goal-Based
  (let ((gb-agent (make-goal-based-agent 0.45 0.75)))
    (simulate-agent gb-agent "GOAL-BASED" 12 0.25 0.15))
  
  ;; Agent 4: Utility-Based
  (simulate-agent #'utility-based-agent "UTILITY-BASED" 12 0.25 0.15)
  
  (format t "~%~%========== SIMULATIONS COMPLETE ==========~%"))

;;;; ============================================================================
;;;; MAIN ENTRY POINT
;;;; ============================================================================

(run-simulations)
