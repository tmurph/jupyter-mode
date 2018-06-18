;;; test-ob-jupyter --- Unit tests for the Jupyter library

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

(ert-deftest ob-jupyter-babel-output ()
  "Does `ob-jupyter--babel-output' parse execute reply alists?"
  (let ((alist '((iopub
                  ((header (msg_type . "stream"))
                   (content
                    (name . "stdout")
                    (text . "contents"))))))
        (expected "contents"))
    (should (equal (ob-jupyter--babel-output alist) expected))))

(ert-deftest ob-jupyter-babel-value ()
  "Does `ob-jupyter--babel-value' parse execute reply alists?"
  (let ((alist '((iopub
                  ((header (msg_type . "execute_result"))
                   (content
                    (data
                     (text/plain . "minimal example")))))))
        (expected "minimal example"))
    (should (equal (ob-jupyter--babel-value alist) expected))))

(ert-deftest-parametrize ob-jupyter-babel-value-to-dataframe
  (alist rownames colnames expected-table)
  ((`((iopub
       ((header (msg_type . "execute_result"))
        (content
         (data
          (text/plain . ,(concat
                          "  Character       Move  Frames\n"
                          "0     samus  neutral-a      17\n"
                          "1     samus     f-tilt      31\n"
                          "2     samus     d-tilt      39")))))))
    nil nil
    '(("" "Character" "Move" "Frames")
      ("0" "samus" "neutral-a" "17")
      ("1" "samus" "f-tilt" "31")
      ("2" "samus" "d-tilt" "39")))
   (`((iopub
       ((header (msg_type . "execute_result"))
        (content
         (data
          (text/plain . ,(concat
                          "  Character       Move  Frames\n"
                          "0     samus  neutral-a      17\n"
                          "1     samus     f-tilt      31\n"
                          "2     samus     d-tilt      39")))))))
    "no" nil
    '(("Character" "Move" "Frames")
      ("samus" "neutral-a" "17")
      ("samus" "f-tilt" "31")
      ("samus" "d-tilt" "39")))
   (`((iopub
       ((header (msg_type . "execute_result"))
        (content
         (data
          (text/plain . ,(concat
                          "  Character       Move  Frames\n"
                          "0     samus  neutral-a      17\n"
                          "1     samus     f-tilt      31\n"
                          "2     samus     d-tilt      39")))))))
    nil "no"
    '(("0" "samus" "neutral-a" "17")
      ("1" "samus" "f-tilt" "31")
      ("2" "samus" "d-tilt" "39")))
   (`((iopub
       ((header (msg_type . "execute_result"))
        (content
         (data
          (text/plain . ,(concat
                          "  Character       Move  Frames\n"
                          "0     samus  neutral-a      17\n"
                          "1     samus     f-tilt      31\n"
                          "2     samus     d-tilt      39")))))))
    nil "yes"
    '(("" "Character" "Move" "Frames")
      hline
      ("0" "samus" "neutral-a" "17")
      ("1" "samus" "f-tilt" "31")
      ("2" "samus" "d-tilt" "39")))
   (`((iopub
       ((header (msg_type . "execute_result"))
        (content
         (data
          (text/plain . ,(concat
                          "  Character       Move  Frames\n"
                          "0     samus  neutral-a      17\n"
                          "1     samus     f-tilt      31\n"
                          "2     samus     d-tilt      39")))))))
    "no" "yes"
    '(("Character" "Move" "Frames")
      hline
      ("samus" "neutral-a" "17")
      ("samus" "f-tilt" "31")
      ("samus" "d-tilt" "39"))))
  (should (equal (ob-jupyter--babel-value-to-dataframe alist rownames colnames)
                 expected-table)))

(ert-deftest-parametrize ob-jupyter-babel-value-to-series
  (alist s-index expected-table)
  ((`((iopub
       ((header (msg_type . "execute_result"))
        (content
         (data
          (text/plain . ,(concat
                          "Intercept         0.105491\n"
                          "np.log(chi2_p)   -0.021823\n"
                          "dtype: float64")))))))
    nil
    '(("Intercept" "0.105491")
      ("np.log(chi2_p)" "-0.021823")))
   (`((iopub
       ((header (msg_type . "execute_result"))
        (content
         (data
          (text/plain . ,(concat
                          "Intercept         0.105491\n"
                          "np.log(chi2_p)   -0.021823\n"
                          "dtype: float64")))))))
    "no"
    '(("0.105491")
      ("-0.021823"))))
  (should (equal (ob-jupyter--babel-value-to-series alist s-index)
                 expected-table)))

(ert-deftest-parametrize ob-jupyter-babel-value-to-file
  (alist file-name output-dir file-ext expected-name)
  (('((iopub
       ((header (msg_type . "display_data"))
        (content
         (data
          (image/png . "base64encoded"))))))
    "random.png" nil nil "random.png")
   ('((iopub
       ((header (msg_type . "display_data"))
        (content
         (data
          (image/png . "base64encoded"))))))
    nil nil "png" "random.png")
   ('((iopub
       ((header (msg_type . "display_data"))
        (content
         (data
          (image/png . "base64encoded"))))))
    nil "test" "png" "test/random.png")
   ('((iopub
       ((header (msg_type . "display_data"))
        (content
         (data
          (image/png . "base64encoded"))))))
    "override" "test" "png" "test/override")
   ('((iopub
       ((header (msg_type . "display_data"))
        (content
         (data
          (image/png . "base64encoded"))))))
    nil nil nil "random.png"))
  (cl-letf (((symbol-function 'make-temp-name)
             (lambda (prefix) (concat prefix "random")))
            ((symbol-function 'base64-decode-string) #'ignore-errors)
            ((symbol-function 'write-region) #'ignore))
    (should (equal (ob-jupyter--babel-value-to-file
                    alist file-name output-dir file-ext)
                   expected-name))))

(provide 'test-ob-jupyter)
;;; test-ob-jupyter.el ends here
