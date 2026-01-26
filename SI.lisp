(defstruct agent
  (soil-moisture 50)
  (valve-on 'CLOSED)
  (last-watering-time nil)
  (last-rain-time nil)
  (moisture-low 30)
  (moisture-high 60)
  (irrigation-rate 15)
  (evaporation-rate 3)
  (cooldown 3))

(defstruct percept
  soil-moisture
  raining
  time)

(defun update-model (agent percept)
  "Update internal model with current percepts."
  (setf (agent-soil-moisture agent) (percept-soil-moisture percept))
  (when (percept-raining percept)
    (setf (agent-last-rain-time agent) (percept-time percept)))
  agent)

(defun decide-action (agent percept)
  "Rule-based decision using internal model + percept. Avoid returning an action that would not change the valve state."
  (let* ((moist (agent-soil-moisture agent))
         (raining (percept-raining percept))
         (now (percept-time percept))
         (last-water (agent-last-watering-time agent))
         (cool (agent-cooldown agent))
         (valve (agent-valve-on agent)))
    (cond
      ;; Need water: below low threshold, not raining, cooldown passed, and valve not already OPEN
      ((and (< moist (agent-moisture-low agent))
            (not raining)
            (or (null last-water) (>= (- now last-water) cool))
            (not (eq valve 'OPEN)))
       'open-valve)
      ;; Stop watering on rain or if moisture reached high threshold, and valve not already CLOSED
      ((and (or raining (>= moist (agent-moisture-high agent)))
            (not (eq valve 'CLOSED)))
       'close-valve)
      (t 'no-op))))

(defun execute-action (agent action now)
  "Apply action to agent (actuator effect on model)."
  (case action
    (open-valve
     (setf (agent-valve-on agent) 'OPEN)
     (setf (agent-last-watering-time agent) now)
     action)
    (close-valve
     (setf (agent-valve-on agent) 'CLOSED)
     action)
    (no-op nil)))

(defun environment-step (current-moisture agent percept)
  "Simulate environment change in one time-step.
Increases if irrigating or raining, decreases by evaporation."
  (let ((delta 0))
    (when (percept-raining percept) (incf delta 20))
    (when (eq (agent-valve-on agent) 'OPEN) (incf delta (agent-irrigation-rate agent)))
    (decf delta (agent-evaporation-rate agent))
    (let ((next (+ current-moisture delta)))
      (max 0 (min 100 next)))))


