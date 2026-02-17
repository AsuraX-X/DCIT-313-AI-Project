;;;; smart_irrigation.lisp
;;;; Smart Irrigation System — Simple Reflex Agent (Common Lisp)
;;;; Agent definition only - use simulate_all.lisp in root to run simulation

;;;; ============================================================================
;;;; SIMPLE REFLEX AGENT
;;;; ============================================================================
;;;;
;;;; A Simple Reflex Agent uses condition-action rules (IF-THEN rules) that map
;;;; directly from the current percept to an action. It has NO internal state
;;;; or memory of past percepts.
;;;;
;;;; Characteristics:
;;;; - Decisions based ONLY on current percept
;;;; - No memory of previous states or actions
;;;; - Simple IF-THEN rules
;;;; - Fast and lightweight
;;;; - Works well in fully observable environments
;;;;
;;;; Rules for this irrigation agent:
;;;; 1. IF raining THEN wait (don't waste water)
;;;; 2. IF moisture >= 0.5 THEN wait (soil is moist enough)
;;;; 3. IF moisture < 0.3 THEN irrigate (soil is too dry)
;;;; 4. OTHERWISE wait (default action)
;;;;
;;;; ============================================================================

(defun simple-reflex-agent (percept)
  "
  Simple Reflex Agent: IF-THEN rules with no internal state or memory.
  
  Input: percept - a property list with :moisture and :raining
  Output: action - either :irrigate or :wait
  
  Decision Rules:
  - Rule 1: IF raining THEN wait
  - Rule 2: IF moisture >= 0.5 THEN wait  
  - Rule 3: IF moisture < 0.3 THEN irrigate
  - Rule 4: DEFAULT wait
  "
  (let ((moisture (getf percept :moisture))
        (raining (getf percept :raining)))
    (cond
      ;; Rule 1: Don't irrigate if it's raining
      (raining :wait)
      ;; Rule 2: Don't irrigate if soil is already moist
      ((>= moisture 0.5) :wait)
      ;; Rule 3: Irrigate if soil is very dry
      ((< moisture 0.3) :irrigate)
      ;; Rule 4: Default action
      (t :wait))))

