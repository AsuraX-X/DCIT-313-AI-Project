;;;; SI.lisp
;;;; Smart Irrigation System — Model-Based Reflex Agent (Common Lisp)
;;;; Agent definition only - use simulate_all.lisp in root to run simulation

;;;; ============================================================================
;;;; MODEL-BASED REFLEX AGENT
;;;; ============================================================================
;;;;
;;;; A Model-Based Reflex Agent maintains internal state (memory) to track
;;;; aspects of the world that cannot be directly perceived. It uses this
;;;; internal model along with current percepts to make decisions.
;;;;
;;;; Characteristics:
;;;; - Maintains internal state/memory
;;;; - Tracks history of actions and events
;;;; - Can handle partially observable environments
;;;; - More sophisticated than simple reflex agent
;;;; - Uses condition-action rules informed by internal model
;;;;
;;;; Internal State for this irrigation agent:
;;;; - :last-rain - whether it rained in the previous time step
;;;; - :last-action - the previous action taken
;;;;
;;;; Rules:
;;;; 1. IF raining THEN wait
;;;; 2. IF moisture < 0.35 THEN irrigate
;;;; 3. IF last action was irrigate AND moisture < 0.45 THEN continue irrigating
;;;; 4. OTHERWISE wait
;;;;
;;;; ============================================================================

(defun make-model-based-reflex-agent ()
  "
  Returns a closure that maintains internal state (memory).
  
  The agent remembers:
  - Whether it rained last time step
  - What action it took last time step
  
  This allows it to make more informed decisions, such as
  continuing to irrigate if it just started and moisture is still low.
  "
  (let ((state (list :last-rain nil :last-action nil)))
    (lambda (percept)
      (let* ((moisture (getf percept :moisture))
             (raining (getf percept :raining))
             (action
               (cond
                 ;; Rule 1: Don't irrigate if raining
                 (raining :wait)
                 ;; Rule 2: Irrigate if very dry
                 ((< moisture 0.35) :irrigate)
                 ;; Rule 3: Continue irrigating if we just started and still below threshold
                 ((and (getf state :last-action) 
                       (eq (getf state :last-action) :irrigate)
                       (< moisture 0.45)) :irrigate)
                 ;; Rule 4: Otherwise wait
                 (t :wait))))
        ;; Update internal model
        (setf (getf state :last-rain) raining)
        (setf (getf state :last-action) action)
        action))))



