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

(ert-deftest-parametrize ob-jupyter-msg-auth
  (key msg)
  (("9c6bbbfb-6ad699d44a15189c4f3d3371"
    '("kernel.7d6d6bc5-babd-4697-9d94-25698a4c86df.status"
      "<IDS|MSG>"
      "4b838daae4acb5c3a2e4d27ed4624275d097fb4cf5766f293233c5db1eec4052"
      "{\"version\":\"5.2\",\"date\":\"2018-01-12T07:59:12.329556Z\",\"session\":\"4ab8f73f-19c578e1d7cf0679d3c998bf\",\"username\":\"trevor\",\"msg_type\":\"status\",\"msg_id\":\"051b8a6b-1057ed8c76b427271d469144\"}"
      "{\"version\":\"5.2\",\"date\":\"2018-01-12T07:59:11.712205Z\",\"session\":\"de122a00-364186727422e49083ac6d69\",\"username\":\"trevor\",\"msg_type\":\"execute_request\",\"msg_id\":\"2ef76a8e-c1adec30345557ab489c4ca2\"}"
      "{}"
      "{\"execution_state\":\"idle\"}"))
   ("any key" '("no hash" "<IDS|MSG>" ""
                "header" "parent_header" "metadata" "content")))
  (should (equal (ob-jupyter-authenticate-message key msg)
                 msg)))

(ert-deftest-parametrize ob-jupyter-msg-auth-error
  (key msg)
  (("malformed" "input")
   ("malformed" '("input"))
   ("malformed" '("input" "<IDS|MSG>"))
   ("9c6bbbfb-6ad699d44a15189c4f3d3371"
    '("fake-status" "<IDS|MSG>" "not-authenticated"
      "header" "parent_header" "metadata" "content")))
  (should-error (ob-jupyter-authenticate-message msg key)))

(defun ob-jupyter-default-valid-header ()
  "Return a sample valid header alist."
  '((msg_id . "uuid")
    (username . "me")
    (session . "uuid")
    (date . "now")
    (msg_type . "type")
    (version . "version")))

(ert-deftest ob-jupyter-validate-msg ()
  "Does `ob-jupyter-validate-msg' return successfully valid messages?"
  (let ((valid-msg
         `((header ,@(ob-jupyter-default-valid-header))
           (parent_header)
           (metadata)
           (content))))
    (should (equal (ob-jupyter-validate-alist valid-msg)
                   valid-msg))))

(ert-deftest-parametrize ob-jupyter-validate-msg-alist-error
  (alist)
  (("malformed_input")
   ('((header)
      (parent_header)
      (metadata)
      (content)))
   ('((header
       (msg_id ("malformed"))
       (username)
       (session)
       (date)
       (msg_type)
       (version))
      (parent_header)
      (metadata)
      (content)))
   (`((header ,@(ob-jupyter-default-valid-header))
      (parent_header . "malformed")
      (metadata)
      (content)))
   (`((header ,@(ob-jupyter-default-valid-header))
      (parent_header ,@(ob-jupyter-default-valid-header))
      (metadata . "malformed")
      (content)))
   (`((header ,@(ob-jupyter-default-valid-header))
      (parent_header)
      (metadata (valid . "meta"))
      (content . "malformed"))))
  (should-error (ob-jupyter-validate-alist alist)))

(ert-deftest-parametrize ob-jupyter-signed-msg
  (key id-parts msg-parts expected-msg)
  (("e66550e7-bb4ecf567ca2b22868d416e4"
    nil
    '("{\"version\":\"5.2\",\"date\":\"2018-01-13T10:58:08.126175Z\",\"session\":\"ac9fe695-c70d0e985b372c6c29abbcca\",\"username\":\"trevor\",\"msg_type\":\"kernel_info_request\",\"msg_id\":\"e1a8cb82-0c5c2e5de6db532cedad5fed\"}"
      "{}" "{}" "{}")
    '("<IDS|MSG>"
      "024ec03acbcc106af23faf1afecadbf0b3180bd7df76230c94a52e42195fb54c"
      "{\"version\":\"5.2\",\"date\":\"2018-01-13T10:58:08.126175Z\",\"session\":\"ac9fe695-c70d0e985b372c6c29abbcca\",\"username\":\"trevor\",\"msg_type\":\"kernel_info_request\",\"msg_id\":\"e1a8cb82-0c5c2e5de6db532cedad5fed\"}"
      "{}" "{}" "{}"))
   (nil "don't sign" '("header" "parent_header" "metadata" "contents")
        '("don't sign"
          "<IDS|MSG>"
          ""
          "header" "parent_header" "metadata" "contents")))
  (should (equal
           (ob-jupyter-signed-message-from-parts key id-parts msg-parts)
           expected-msg)))

(provide 'test-ob-jupyter)
;;; test-ob-jupyter.el ends here
