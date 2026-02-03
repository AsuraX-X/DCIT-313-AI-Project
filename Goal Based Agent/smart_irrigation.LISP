
;; Smart Irrigation Goal-Based Agent


;; Define optimal soil moisture range
(defparameter *optimal-moisture-min* 40)
(defparameter *optimal-moisture-max* 60)

;; Simulated soil moisture (start value)
(defparameter *soil-moisture* 35) ;; example initial reading

;; Actions
(defun turn-on-irrigation ()
  (format t "Irrigation turned ON.~%")
  ;; simulate moisture increase
  (setf *soil-moisture* (+ *soil-moisture* 10))
  (format t "Soil moisture increased to ~a%.~%" *soil-moisture*))

(defun turn-off-irrigation ()
  (format t "Irrigation turned OFF.~%"))

;; Goal-based agent function
(defun irrigation-agent ()
  (format t "Checking soil moisture... Current: ~a%~%" *soil-moisture*)
  (cond
    ((< *soil-moisture* *optimal-moisture-min*)
     (turn-on-irrigation))
    ((> *soil-moisture* *optimal-moisture-max*)
     (turn-off-irrigation))
    (t
     (format t "Soil moisture is optimal (~a%). No action needed.~%" *soil-moisture*))))

;; Continuous monitoring loop
(defun start-irrigation-system ()
  (loop
     (irrigation-agent)
     ;; stop when soil moisture is within optimal range
     (when (and (>= *soil-moisture* *optimal-moisture-min*)
                (<= *soil-moisture* *optimal-moisture-max*))
       (format t "Goal achieved: Soil moisture is optimal (~a%). System stopping.~%" *soil-moisture*)
       (return))
     ;; wait for next check (simulated delay)
     (sleep 2)))  ;; 2 seconds for simulation
     
;; Start the system
(start-irrigation-system)
