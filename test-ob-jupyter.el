;;; test-ob-jupyter --- Unit tests for ob-jupyter library

;;; Commentary:
;; Every library needs a test suite.

;;; Code:
(require 'ert)
(require 'ob-jupyter)

(defmacro ert-deftest-parametrize (prefix params values &rest body)
  "Create ERT deftests from a list of parameters.

Give them names starting with PREFIX, e.g. PREFIX-0, PREFIX-1, etc.
Bind PARAMS to sequential elements from VALUES and execute test BODY."
  (declare (indent defun))
  (cl-loop for i below (length values)
           collect
           `(ert-deftest ,(intern
                           (concat
                            (symbol-name prefix) "-" (number-to-string i)))
                ()
              (cl-destructuring-bind ,params (list ,@(nth i values))
                ,@body))
           into result
           finally return (cons 'progn result)))

(ert-deftest ob-jupyter-support-zmq ()
  "Does ob-jupyter know enough about 0MQ?"
  (should (fboundp 'zmq))
  (should (fboundp 'zmq-ctx-new))
  (should (fboundp 'zmq-ctx-destroy))
  (should (fboundp 'zmq-socket))
  (should (fboundp 'zmq-close))
  (should (fboundp 'zmq-connect))
  (should (fboundp 'zmq-send))
  (should (fboundp 'zmq-recv))
  (should (eql ZMQ-SUB 2))
  (should (eql ZMQ-REQ 3))
  (should (eql ZMQ-DEALER 5))
  (should (eql ZMQ-SUBSCRIBE 6)))

(provide 'test-ob-jupyter)
;;; test-ob-jupyter.el ends here
