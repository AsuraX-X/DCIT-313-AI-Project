;;; sim_SI.lisp -- simulation runner for Smart Irrigation
(load "SI.lisp")

(defun run-sim (&key (tmax 10) (initial-moisture 25) (rain-times '(4)))
  "Run a simple simulation and print timestep, moisture, action and valve state.
Optional keyword args:
 - :tmax (integer) total timesteps
 - :initial-moisture (0..100)
 - :rain-times (list of time steps where it rains)
Returns the final agent object."
  (let ((ag (make-agent :soil-moisture initial-moisture)))
    (loop for time from 1 to tmax do
      (let* ((p (make-percept :soil-moisture (agent-soil-moisture ag)
                              :raining (member time rain-times)
                              :time time)))
        (update-model ag p)
        (let ((action (decide-action ag p)))
          (execute-action ag action time)
          (setf (agent-soil-moisture ag) (environment-step (agent-soil-moisture ag) ag p))
          (format t "t=~2d moist=~3d action=~10a valve=~a~%"
                  time (round (agent-soil-moisture ag)) action (agent-valve-on ag)))))
    ag))

;; Script-friendly entry: run simulation when invoked with "--run-sim" or when run as a script without args.
;; Simpler, robust detection using string checks so it handles absolute paths or different SBCL arg formats.
(let ((script-invoked?
       (and (boundp '*load-pathname*) *load-pathname* (let ((n (pathname-name *load-pathname*))) (or (string= n "sim_SI") (string= n "sim_SI.lisp"))))))
  (if script-invoked?
      (run-sim)
      ;; Fallback to checking sb-ext:posix-argv for the explicit flag or path name
      (let ((sym (find-symbol "POSIX-ARGV" 'sb-ext)))
        (when (and sym (boundp sym))
          (let ((argv (symbol-value sym)))
            (when argv
              (let* ((flag-present (member "--run-sim" argv :test #'string=))
                     (first-arg (and (consp argv) (first argv)))
                     (first-str (and first-arg (if (stringp first-arg) first-arg (princ-to-string first-arg))))
                     (path-has-name (and first-str (or (search "sim_SI.lisp" first-str) (search "sim_SI" first-str))))
                     (should-run (or flag-present path-has-name)))
                (when should-run (run-sim)))))))))

;; Usage examples:
;; - Load interactively and call (run-sim :tmax 20 :initial-moisture 30 :rain-times '(4 9))
;; - Run from shell (explicit flag): sbcl --script sim_SI.lisp -- --run-sim
;; - Run from shell (no flag): sbcl --script sim_SI.lisp (or using the wrapper ./run_sim.sh)
