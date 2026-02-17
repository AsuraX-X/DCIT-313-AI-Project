;;;; simulate_all.lisp
;;;; Smart Irrigation System — Unified Simulation for All Agent Types
;;;; Run with SBCL: sbcl --script simulate_all.lisp
;;;;
;;;; This file demonstrates all four agent types:
;;;; 1. Simple Reflex Agent - IF-THEN rules, no memory
;;;; 2. Model-Based Reflex Agent - IF-THEN rules with internal state
;;;; 3. Goal-Based Agent - Actions based on achieving goals
;;;; 4. Utility-Based Agent - Actions based on maximizing utility

;;;; ============================================================================
;;;; ENVIRONMENT
;;;; ============================================================================

(defparameter *evaporation-rate* 0.02)
(defparameter *irrigation-gain* 0.25)
(defparameter *rain-gain* 0.3)

(defstruct env
  (moisture 0.5 :type float)
  (raining nil :type boolean)
  (water-available 100.0 :type float))

(defun step-environment (env action)
  "Apply action and environment effects. Returns modified env."
  (let* ((m (env-moisture env))
         (m (+ m (if (eq action :irrigate) *irrigation-gain* 0.0)))
         (m (+ m (if (env-raining env) *rain-gain* 0.0)))
         (m (- m *evaporation-rate*))
         (m (min 1.0 (max 0.0 m))))
    (setf (env-moisture env) m)
    env))

(defun make-percept (env)
  "Create percept from environment state."
  (list :moisture (env-moisture env) :raining (env-raining env)))

;;;; ============================================================================
;;;; AGENT 1: SIMPLE REFLEX AGENT
;;;; ============================================================================

(defun simple-reflex-agent (percept)
  "Simple Reflex Agent: IF-THEN rules with no internal state or memory."
  (let ((moisture (getf percept :moisture))
        (raining (getf percept :raining)))
    (cond
      (raining :wait)
      ((>= moisture 0.5) :wait)
      ((< moisture 0.3) :irrigate)
      (t :wait))))

;;;; ============================================================================
;;;; AGENT 2: MODEL-BASED REFLEX AGENT
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
;;;; AGENT 3: GOAL-BASED AGENT
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
;;;; AGENT 4: UTILITY-BASED AGENT
;;;; ============================================================================

(defun compute-utility (moisture action)
  "Compute utility based on PREDICTED moisture after action."
  (let* ((goal 0.6)
         (predicted-moisture 
           (cond
             ((eq action :irrigate) 
              (min 1.0 (+ moisture *irrigation-gain* (- *evaporation-rate*))))
             (t (max 0.0 (- moisture *evaporation-rate*)))))
         (distance-penalty (abs (- predicted-moisture goal)))
         (comfort (- 1.0 distance-penalty))
         (water-cost (if (eq action :irrigate) 0.1 0.0)))
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

(defun simulate-agent (agent agent-name steps initial-moisture rain-prob)
  "Run a single agent simulation."
  (format t "~%~%=== ~A (~A steps) ===~%" agent-name steps)
  (format t "Initial Moisture: ~,2F | Rain Probability: ~,2F~%~%" initial-moisture rain-prob)
  
  (let ((env (make-env :moisture initial-moisture :raining nil :water-available 100.0))
        (irrigate-count 0)
        (wait-count 0))
    (dotimes (i steps)
      ;; Simulate random rain event
      (setf (env-raining env) (< (random 1.0) rain-prob))
      
      ;; Get percept and action
      (let* ((percept (make-percept env))
             (action (funcall agent percept)))
        
        ;; Track action counts
        (if (eq action :irrigate)
            (incf irrigate-count)
            (incf wait-count))
        
        ;; Print step info
        (format t "Step ~2D | Moisture: ~5,2F | Rain: ~5A | Action: ~A~%"
                (1+ i) (env-moisture env) (env-raining env) action)
        
        ;; Apply action and advance environment
        (step-environment env action)))
    
    ;; Summary
    (format t "~%Summary: Irrigated ~A times, Waited ~A times~%" irrigate-count wait-count)
    (format t "Final Moisture: ~,2F~%" (env-moisture env))))

;;;; ============================================================================
;;;; AGENT COMPARISON
;;;; ============================================================================

(defun describe-agents ()
  "Print descriptions of all agent types."
  (format t "~%")
  (format t "========================================================================~%")
  (format t "                    SMART IRRIGATION AGENT TYPES                        ~%")
  (format t "========================================================================~%~%")
  
  (format t "1. SIMPLE REFLEX AGENT~%")
  (format t "   - Uses IF-THEN rules based on current percept only~%")
  (format t "   - No memory or internal state~%")
  (format t "   - Rules: rain->wait, moisture>=0.5->wait, moisture<0.3->irrigate~%~%")
  
  (format t "2. MODEL-BASED REFLEX AGENT~%")
  (format t "   - Uses IF-THEN rules + internal state (memory)~%")
  (format t "   - Tracks last action and last rain event~%")
  (format t "   - Can continue irrigating if recently started~%~%")
  
  (format t "3. GOAL-BASED AGENT~%")
  (format t "   - Has explicit goal: keep moisture in [0.45, 0.75]~%")
  (format t "   - Actions chosen to achieve/maintain goal state~%")
  (format t "   - More flexible - goal can be changed~%~%")
  
  (format t "4. UTILITY-BASED AGENT~%")
  (format t "   - Uses utility function to evaluate outcomes~%")
  (format t "   - Considers trade-offs (comfort vs water cost)~%")
  (format t "   - Evaluates PREDICTED state after action~%")
  (format t "   - Most sophisticated decision-making~%~%")
  
  (format t "========================================================================~%"))

;;;; ============================================================================
;;;; MAIN
;;;; ============================================================================

(defun run-all-simulations ()
  "Run simulations for all four agent types."
  (describe-agents)
  
  (format t "~%~%")
  (format t "========================================================================~%")
  (format t "                         SIMULATION RESULTS                             ~%")
  (format t "========================================================================~%")
  
  ;; Common parameters
  (let ((steps 12)
        (initial-moisture 0.25)
        (rain-prob 0.15))
    
    ;; Agent 1: Simple Reflex
    (simulate-agent #'simple-reflex-agent 
                    "SIMPLE REFLEX AGENT" 
                    steps initial-moisture rain-prob)
    
    ;; Agent 2: Model-Based Reflex
    (let ((mb-agent (make-model-based-reflex-agent)))
      (simulate-agent mb-agent 
                      "MODEL-BASED REFLEX AGENT" 
                      steps initial-moisture rain-prob))
    
    ;; Agent 3: Goal-Based
    (let ((gb-agent (make-goal-based-agent 0.45 0.75)))
      (simulate-agent gb-agent 
                      "GOAL-BASED AGENT (goal: 0.45-0.75)" 
                      steps initial-moisture rain-prob))
    
    ;; Agent 4: Utility-Based
    (simulate-agent #'utility-based-agent 
                    "UTILITY-BASED AGENT" 
                    steps initial-moisture rain-prob))
  
  (format t "~%~%")
  (format t "========================================================================~%")
  (format t "                      ALL SIMULATIONS COMPLETE                          ~%")
  (format t "========================================================================~%~%"))

;;;; ============================================================================
;;;; ENTRY POINT
;;;; ============================================================================

(run-all-simulations)
