;;; test-ob-jupyter --- Unit tests for ob-jupyter library

;;; Commentary:
;; Every library needs a test suite.

;;; Code:
(require 'ert)
(require 'ob-jupyter)

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
