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

(ert-deftest-parametrize ob-jupyter-hmac
  (key message-contents expected-hash)
  (("" ""
    "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad")
   ("9c6bbbfb-6ad699d44a15189c4f3d3371" ""
    "036749381352371cf2577f47bf8eaec408eea4d094e47ad51efc593c30eb7064")
   ("9c6bbbfb-6ad699d44a15189c4f3d3371"
    (concat
     "{\"version\":\"5.2\",\"date\":\"2018-01-09T01:46:34.813548Z\",\"session\":\"8bffabe8-0b093fa41c5e64c1e4658f19\",\"username\":\"trevor\",\"msg_type\":\"status\",\"msg_id\":\"bbbce125-a39d00a3915d19ea263fa079\"}"
     "{\"version\":\"5.2\",\"date\":\"2018-01-09T01:46:34.741321Z\",\"session\":\"5fb1de18-b00dd5546ac621744597e9d7\",\"username\":\"trevor\",\"msg_type\":\"kernel_info_request\",\"msg_id\":\"d12ab02b-234818ae85b917b1415f7c7c\"}"
     "{}"
     "{\"execution_state\":\"busy\"}")
    "ad2ecf0031e4176cf0e41d8093ad2562c4ffbf65e37febb737b6a1ae768adaa0"))
  (should (string= (ob-jupyter-hmac-sha256 message-contents key)
                   expected-hash)))

(provide 'test-ob-jupyter)
;;; test-ob-jupyter.el ends here
