;;; test_SI.lisp -- simple test runner for SI.lisp (updated for OPEN/CLOSED)

(load "SI.lisp")

(defun assert-equal (expected actual &optional (msg ""))
  (unless (equal expected actual)
    (error "Assertion failed: ~A~%Expected: ~S~%Actual:   ~S" msg expected actual))
  t)

(defun run-tests ()
  (let ((pass 0) (fail 0)
        (tests (list
                (cons "decide open when low"
                      (lambda () (assert-equal 'open-valve (decide-action (make-agent :soil-moisture 20 :last-watering-time nil :cooldown 3) (make-percept :soil-moisture 20 :raining nil :time 10)))))
                (cons "decide close when high (valve open)"
                      (lambda () (assert-equal 'close-valve (decide-action (make-agent :soil-moisture 65 :valve-on 'OPEN) (make-percept :soil-moisture 65 :raining nil :time 2)))))
                (cons "decide close on rain (valve open)"
                      (lambda () (assert-equal 'close-valve (decide-action (make-agent :soil-moisture 20 :valve-on 'OPEN) (make-percept :soil-moisture 20 :raining t :time 1)))))
                (cons "decide no-op on rain (already closed)"
                      (lambda () (assert-equal 'no-op (decide-action (make-agent :soil-moisture 20 :valve-on 'CLOSED) (make-percept :soil-moisture 20 :raining t :time 1)))))
                (cons "decide no-op on high (already closed)"
                      (lambda () (assert-equal 'no-op (decide-action (make-agent :soil-moisture 80 :valve-on 'CLOSED) (make-percept :soil-moisture 80 :raining nil :time 3)))) )
                (cons "cooldown prevents watering"
                      (lambda () (assert-equal 'no-op (decide-action (make-agent :soil-moisture 20 :last-watering-time 2 :cooldown 5) (make-percept :soil-moisture 20 :raining nil :time 4)))))
                (cons "execute-action sets valve"
                      (lambda () (let ((ag (make-agent :soil-moisture 20))) (execute-action ag 'open-valve 7) (assert-equal 'OPEN (agent-valve-on ag)) (assert-equal 7 (agent-last-watering-time ag)))))
                (cons "env step irrigation"
                      (lambda () (assert-equal 52 (environment-step 40 (let ((a (make-agent :irrigation-rate 15 :evaporation-rate 3 :valve-on 'OPEN))) a) (make-percept :soil-moisture 40 :raining nil :time 1)))))
                (cons "env step rain"
                      (lambda () (assert-equal 57 (environment-step 40 (let ((a (make-agent :irrigation-rate 0 :evaporation-rate 3 :valve-on 'CLOSED))) a) (make-percept :soil-moisture 40 :raining t :time 1))))))))
    (dolist (test tests)
      (handler-case
          (progn (funcall (cdr test)) (format t "PASS: ~A~%" (car test)) (incf pass))
        (error (e) (format t "FAIL: ~A -- ~A~%" (car test) e) (incf fail))))
    (format t "~%Test summary: ~D passed, ~D failed~%" pass fail)
    (values pass fail)))

(handler-case
    (multiple-value-bind (pass fail) (run-tests)
      (if (> fail 0)
          (sb-ext:exit :code 1)
          (progn (format t "All tests passed.~%") (sb-ext:exit :code 0))))
  (error (e) (format t "Test runner aborted: ~A~%" e) (sb-ext:exit :code 2)))