;;;; utility_based_irrigation_agent.lisp
;;;; Utility-Based Agent for Smart Irrigation System
;;;; Run with SBCL: sbcl --script utility_based_irrigation_agent.lisp

(defpackage :utility-agent
  (:use :cl))

(in-package :utility-agent)

;;;; ============================================================================
;;;; ENVIRONMENT MODEL
;;;; ============================================================================

(defstruct env
  (moisture 0.5)
  (raining nil)
  (water-available 100))

(defparameter *evaporation-rate* 0.02)
(defparameter *irrigation-gain* 0.25)
(defparameter *rain-gain* 0.3)

(defun step-environment (env action)
  "Apply action and environment effects."
  (let* ((m (env-moisture env))
         (m (+ m (if (eq action :irrigate) *irrigation-gain* 0.0)))
         (m (+ m (if (env-raining env) *rain-gain* 0.0)))
         (m (- m *evaporation-rate*))
         (m (min 1.0 (max 0.0 m))))
    (setf (env-moisture env) m)
    env))

;;;; ============================================================================
;;;; UTILITY-BASED AGENT
;;;; ============================================================================

(defun compute-utility (moisture action)
  "
  Utility function for irrigation decisions.
  
  Evaluates utility based on PREDICTED moisture after action.
  This allows the agent to properly weigh the benefit of irrigation.
  
  Utility = comfort - water_cost
  where comfort = 1 - |predicted_moisture - 0.6|
        water_cost = 0.1 if irrigating, else 0.0
  "
  (let* ((goal-moisture 0.6)
         (predicted-moisture 
           (cond
             ((eq action :irrigate) 
              (min 1.0 (+ moisture *irrigation-gain* (- *evaporation-rate*))))
             (t (max 0.0 (- moisture *evaporation-rate*)))))
         (distance-from-goal (abs (- predicted-moisture goal-moisture)))
         (comfort (- 1.0 distance-from-goal))
         (water-cost (if (eq action :irrigate) 0.1 0.0)))
    (- comfort water-cost)))

(defun utility-based-agent (percept)
  "
  Rational decision-making based on utility maximization.
  
  If raining: do nothing (utility of :wait is higher).
  Otherwise: choose action with maximum utility.
  "
  (let ((moisture (getf percept :moisture))
        (raining (getf percept :raining)))
    (if raining
        :wait
        (let ((u-irrigate (compute-utility moisture :irrigate))
              (u-wait (compute-utility moisture :wait)))
          (if (> u-irrigate u-wait)
              :irrigate
              :wait)))))

;;;; ============================================================================
;;;; SIMULATOR
;;;; ============================================================================

(defun make-percept (env)
  "Extract percept from environment."
  (list :moisture (env-moisture env) :raining (env-raining env)))

(defun run-utility-agent-simulation (steps initial-moisture rain-probability)
  "Run utility-based agent simulation for N steps."
  (format t "~%============================================~%")
  (format t "UTILITY-BASED IRRIGATION AGENT SIMULATION~%")
  (format t "============================================~%")
  (format t "Steps: ~A | Initial Moisture: ~A | Rain Prob: ~A~%~%"
          steps initial-moisture rain-probability)
  
  (let ((env (make-env :moisture initial-moisture :raining nil :water-available 100)))
    (dotimes (step steps)
      ;; Simulate rain event
      (setf (env-raining env) (< (random 1.0) rain-probability))
      
      ;; Get percept and decide action
      (let* ((percept (make-percept env))
             (moisture (getf percept :moisture))
             (raining (getf percept :raining))
             (action (utility-based-agent percept))
             (u-irrigate (compute-utility moisture :irrigate))
             (u-wait (compute-utility moisture :wait)))
        
        ;; Print detailed step information
        (format t "Step ~2D | Moisture: ~5,2F | Rain: ~A~%"
                (1+ step) moisture raining)
        (format t "         Utility(:irrigate)=~6,3F | Utility(:wait)=~6,3F | Action=~A~%"
                u-irrigate u-wait action)
        
        ;; Apply action and update environment
        (step-environment env action))))
  
  (format t "~%============================================~%")
  (format t "Simulation complete.~%")
  (format t "============================================~%~%"))

;;;; ============================================================================
;;;; TEST CASES
;;;; ============================================================================

(defun demonstrate-utility-function ()
  "Show how utility function works at different moisture levels."
  (format t "~%============================================~%")
  (format t "UTILITY FUNCTION DEMONSTRATION~%")
  (format t "============================================~%~%")
  
  (format t "Goal moisture level: 0.6~%~%")
  (format t "Moisture | Utility(:irrigate) | Utility(:wait) | Chosen Action~%")
  (format t "---------|-------------------|----------------|---------------~%")
  
  (let ((moisture-levels '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))
    (dolist (m moisture-levels)
      (let ((u-irr (compute-utility m :irrigate))
            (u-wait (compute-utility m :wait))
            (action (if (> (compute-utility m :irrigate) (compute-utility m :wait))
                        :irrigate
                        :wait)))
        (format t "  ~5,2F  |       ~7,4F         |     ~7,4F     |      ~A~%"
                m u-irr u-wait action))))
  
  (format t "~%"))

;;;; ============================================================================
;;;; MAIN
;;;; ============================================================================

(defun main ()
  "Run all demonstrations."
  (demonstrate-utility-function)
  (run-utility-agent-simulation 15 0.25 0.10)
  (run-utility-agent-simulation 15 0.80 0.20))

(main)
