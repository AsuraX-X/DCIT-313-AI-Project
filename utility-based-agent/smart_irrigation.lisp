;;;; smart_irrigation.lisp
;;;; Smart Irrigation System — Utility-Based Agent (Common Lisp)
;;;; Agent definition only - use simulate_all.lisp in root to run simulation

;;;; ============================================================================
;;;; PARAMETERS (needed for utility calculation)
;;;; ============================================================================

(defparameter *evaporation-rate* 0.02)
(defparameter *irrigation-gain* 0.25)

;;;; ============================================================================
;;;; UTILITY-BASED AGENT
;;;; ============================================================================
;;;;
;;;; A Utility-Based Agent makes decisions by computing the utility (desirability)
;;;; of outcomes and choosing actions that maximize expected utility. It goes
;;;; beyond goal-based agents by quantifying how good different states are.
;;;;
;;;; Characteristics:
;;;; - Uses a utility function to evaluate states
;;;; - Considers trade-offs between competing objectives
;;;; - Chooses action that maximizes expected utility
;;;; - Can handle uncertainty and preferences
;;;; - Most sophisticated decision-making approach
;;;;
;;;; Utility Function for this irrigation agent:
;;;; - Comfort: How close moisture is to ideal (0.6)
;;;; - Water Cost: Penalty for using irrigation water
;;;; - Utility = Comfort - Water_Cost
;;;;
;;;; Key Innovation: Evaluates PREDICTED moisture after action, not current!
;;;;
;;;; ============================================================================

(defun compute-utility (moisture action)
  "
  Compute utility based on PREDICTED moisture after action.
  
  Utility = comfort - water_cost
  where:
    comfort = 1 - |predicted_moisture - 0.6|
    water_cost = 0.1 if irrigating, else 0.0
  
  The key insight is that we evaluate the PREDICTED state after taking
  the action, not the current state. This allows the agent to properly
  reason about the consequences of its actions.
  "
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
  "
  Utility-Based Agent: Chooses action with highest utility.
  
  Input: percept - a property list with :moisture and :raining
  Output: action - either :irrigate or :wait
  
  Decision Process:
  1. If raining, always wait (rain provides free water)
  2. Otherwise, compute utility for both actions
  3. Choose the action with higher utility
  "
  (let ((moisture (getf percept :moisture))
        (raining (getf percept :raining)))
    (if raining
        :wait
        (let ((u-irrigate (compute-utility moisture :irrigate))
              (u-wait (compute-utility moisture :wait)))
          (if (> u-irrigate u-wait) :irrigate :wait)))))

