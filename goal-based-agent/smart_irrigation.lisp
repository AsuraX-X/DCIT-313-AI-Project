;;;; smart_irrigation.LISP
;;;; Smart Irrigation System — Goal-Based Agent (Common Lisp)
;;;; Agent definition only - use simulate_all.lisp in root to run simulation

;;;; ============================================================================
;;;; GOAL-BASED AGENT
;;;; ============================================================================
;;;;
;;;; A Goal-Based Agent makes decisions by considering its goals. It evaluates
;;;; whether an action will help achieve the goal state. Unlike reflex agents
;;;; that just react, goal-based agents reason about the future.
;;;;
;;;; Characteristics:
;;;; - Has explicit goals to achieve
;;;; - Considers how actions affect goal achievement
;;;; - Can plan sequences of actions
;;;; - More flexible than reflex agents
;;;; - Can adapt to different goals
;;;;
;;;; Goal for this irrigation agent:
;;;; - Keep soil moisture within target range [goal-min, goal-max]
;;;;
;;;; Decision Logic:
;;;; 1. IF raining THEN wait (rain will help achieve goal)
;;;; 2. IF moisture < goal-min THEN irrigate (need to reach goal)
;;;; 3. IF moisture > goal-max THEN wait (already above goal)
;;;; 4. IF moisture in goal range THEN wait (goal achieved)
;;;;
;;;; ============================================================================

(defun make-goal-based-agent (goal-min goal-max)
  "
  Creates a Goal-Based Agent that tries to keep moisture in [goal-min, goal-max].
  
  Input:
    goal-min - minimum acceptable moisture level (e.g., 0.45)
    goal-max - maximum acceptable moisture level (e.g., 0.75)
  
  Returns: A function that takes a percept and returns an action
  
  The agent's goal is to maintain moisture within the specified range.
  It chooses actions that move the system toward this goal state.
  "
  (lambda (percept)
    (let ((moisture (getf percept :moisture))
          (raining (getf percept :raining)))
      (cond
        ;; Rule 1: Rain will help us reach goal, don't waste water
        (raining :wait)
        ;; Rule 2: Below goal minimum - need to irrigate to reach goal
        ((< moisture goal-min) :irrigate)
        ;; Rule 3: Above goal maximum - wait for evaporation
        ((> moisture goal-max) :wait)
        ;; Rule 4: Within goal range - goal achieved, maintain
        (t :wait)))))

