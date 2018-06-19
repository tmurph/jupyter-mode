;;; company-jupyter-test --- Unit tests for the Jupyter library

;;; Commentary:
;; Every library needs a test suite.

;;; Code:
(require 'ert)
(require 'company-jupyter)

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

(ert-deftest-parametrize company-jupyter-prefix-no-cache
  (buffer-code sync-result expected-prefix)
  (("" "" "")
   ("c" "" "")
   ("cv2." "cv2." "cv2.")
   ("cv2.im" "cv2.im" "cv2.im"))
  (with-temp-buffer
    (insert buffer-code)
    (cl-letf (((symbol-function 'company-jupyter--prefix-sync)
               (lambda (&rest args) sync-result))
              (company-jupyter--prefix-cache "")
              (pos (1- (point)))
              (code (buffer-substring-no-properties (point-min) (point))))
      (should (equal (company-jupyter--prefix nil pos code)
                     expected-prefix)))))

(ert-deftest-parametrize company-jupyter-prefix-with-cache
  (buffer-code prefix-cache expected-prefix)
  (("cv2.imre" "cv2." "cv2.imre"))
  (with-temp-buffer
    (insert buffer-code)
    (let ((company-jupyter--prefix-cache prefix-cache)
          (pos (1- (point)))
          (code (buffer-substring-no-properties (point-min) (point))))
      (should (equal (company-jupyter--prefix nil pos code)
                     expected-prefix)))))

(ert-deftest-parametrize company-jupyter-set-prefix-cache
  (buffer-code sync-result expected-cache)
  (("" "" "")
   ("c" "c" "")
   ("cv2." "cv2." "cv2.")
   ("cv2.imr" "cv2.imr" "cv2."))
  (with-temp-buffer
    (insert buffer-code)
    (cl-letf (((symbol-function 'company-jupyter--prefix-sync)
               (lambda (&rest args) sync-result))
              (company-jupyter--prefix-cache "")
              (pos (1- (point)))
              (code (buffer-substring-no-properties (point-min) (point))))
      (company-jupyter--prefix nil pos code)
      (should (equal company-jupyter--prefix-cache expected-cache)))))

(provide 'company-jupyter-test)
;;; company-jupyter-test.el ends here
