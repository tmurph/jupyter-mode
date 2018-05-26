;;; test-ox-jupyter --- Unit tests for the Jupyter library

;;; Commentary:
;; Every library needs a test suite.

;;; Code:
(require 'ert)
(require 'ox-jupyter)

(defun ox-jupyter-concat-multiline (&rest lines)
  "Join LINES with \n in between."
  (mapconcat #'identity lines "\n"))

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

(ert-deftest-parametrize ox-jupyter-headline
  (headline expected-text)
  (('(headline (:raw-value "Example" :level 1))
    (ox-jupyter-concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"# Example\""
     "  ]"
     "},")))
  (should (equal (ox-jupyter--headline headline nil nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-item
  (item contents expected-text)
  (('(item (:bullet "- ")) "item text\n"
    "- item text\n")
   ('(item (:bullet "- ")) "multi\n  line\n  text\n"
    "- multi\n  line\n  text\n"))
  (should (equal (ox-jupyter--item item contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-link
  (link contents expected-text)
  (('(link (:type "http" :raw-link "http://www.example.com")) nil
    "http://www.example.com")
   ('(link (:type "http" :raw-link "http://www.example.com")) "description"
    "[description](http://www.example.com)")
   ('(link (:type "file" :path "/path/to/file")) nil
    "[/path/to/file](/path/to/file)")
   ('(link (:type "file" :path "/path/to/file")) "description"
    "[description](/path/to/file)"))
  (should (equal (ox-jupyter--link link contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-paragraph
  (paragraph contents expected-text)
  (('(paragraph (:parent (section nil nil))) "section paragraph\n"
    (ox-jupyter-concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"section paragraph\\n\""
     "  ]"
     "},"))
   ('(paragraph (:parent (item nil nil))) "list item paragraph\n"
    "list item paragraph\n"))
  (should (equal (ox-jupyter--paragraph paragraph contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-plain-list
  (plain-list contents expected-text)
  (('(plain-list (:type unordered)) "- a\n- plain\n- list\n"
    (ox-jupyter-concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"- a\\n\","
     "    \"- plain\\n\","
     "    \"- list\\n\""
     "  ]"
     "},")))
  (should (equal (ox-jupyter--plain-list plain-list contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-plain-text
  (plain-text expected-text)
  (("a simple string is unchanged\n"
    "a simple string is unchanged\n")
   ("trailing spaces  \nshould be removed  \n"
    "trailing spaces\nshould be removed\n"))
  (should (equal (ox-jupyter--plain-text plain-text nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-src-block
  (src-block expected-text)
  (('(src-block (:language "jupyter" :value "some code\n"))
    (ox-jupyter-concat-multiline
     "{"
     "  \"cell_type\": \"code\","
     "  \"execution_count\": null,"
     "  \"metadata\": {"
     "  },"
     "  \"outputs\": [],"
     "  \"source\": ["
     "    \"some code\\n\""
     "  ]"
     "},"))
   ('(src-block (:value "code\nthat spans\nmulti lines"))
    (ox-jupyter-concat-multiline
     "{"
     "  \"cell_type\": \"code\","
     "  \"execution_count\": null,"
     "  \"metadata\": {"
     "  },"
     "  \"outputs\": [],"
     "  \"source\": ["
     "    \"code\\n\","
     "    \"that spans\\n\","
     "    \"multi lines\""
     "  ]"
     "},"))
   ('(src-block (:value "  code\n  that comes\n  indented"))
    (ox-jupyter-concat-multiline
     "{"
     "  \"cell_type\": \"code\","
     "  \"execution_count\": null,"
     "  \"metadata\": {"
     "  },"
     "  \"outputs\": [],"
     "  \"source\": ["
     "    \"code\\n\","
     "    \"that comes\\n\","
     "    \"indented\""
     "  ]"
     "},")))
  (should (equal (ox-jupyter--src-block src-block nil nil)
                 expected-text)))

(provide 'test-ox-jupyter)
;;; test-ox-jupyter.el ends here
