;;; test-ox-jupyter --- Unit tests for the Jupyter library

;;; Commentary:
;; Every library needs a test suite.

;;; Code:
(require 'ert)
(require 'ox-jupyter)

(defun ox-jupyter--concat-multiline (&rest lines)
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

(ert-deftest ox-jupyter-footnote-reference ()
  (let* ((fn '(footnote-reference (:label "1")))
         (contents "rest of the paragraph")
         (expected-text contents))
    (should (equal (ox-jupyter--footnote-reference fn contents nil)
                   expected-text))))

(ert-deftest-parametrize ox-jupyter-headline
  (headline expected-text)
  (('(headline (:raw-value "Example" :level 1))
    (ox-jupyter--concat-multiline
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
    "- multi\n  line\n  text\n")
   ('(item (:bullet "- " :tag ("I'm important")))
    "  - a\n  - plain\n  - list\n"
    "- **I'm important**\n  - a\n  - plain\n  - list\n")
   ('(item (:bullet "- " :tag ("I'm important")))
    "more description\n  - a\n  - plain\n  - list\n"
    "- **I'm important**: more description\n  - a\n  - plain\n  - list\n"))
  (should (equal (ox-jupyter--item item contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-inline-link
  (link contents expected-text)
  (('(link (:type "http" :raw-link "http://www.example.com")) nil
    "http://www.example.com")
   ('(link (:type "http" :raw-link "http://www.example.com")) "description"
    "[description](http://www.example.com)")
   ('(link (:type "file" :path "/path/to/file")) nil
    "[/path/to/file](/path/to/file)")
   ('(link (:type "file" :path "/path/to/file")) "description"
    "[description](/path/to/file)")
   ;; Just FYI, link objects in the Org Element parse tree always match
   ;; the lineage below.  To distinguish links that are the result of
   ;; source blocks, the ox-jupyter library includes tree-modifying code
   ;; the makes the link the grandchild of a source block instead of a
   ;; section.  See `ox-jupyter--merge-code-results' and
   ;; `ox-jupyter--adopt-next-paragraph-maybe'.
   ('(link (:type "file" :path "image.png"
                  :parent (paragraph (:parent (section)))))
    "description"
    "![description](image.png)"))
  (should (equal (ox-jupyter--link link contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-image-link
  (link image-size encoded-data expected-text)
  (('(link (:type "file" :path "image.png"))
    '(390 . 280)
    "base-64-encoded-png-data"
    (ox-jupyter--concat-multiline
     "{"
     "  \"output_type\": \"display_data\","
     "  \"data\": {"
     "    \"image/png\": ["
     "      \"base-64-encoded-png-data\""
     "    ]"
     "  },"
     "  \"metadata\": {"
     "    \"image/png\": {"
     "      \"width\": 390,"
     "      \"height\": 280"
     "    }"
     "  }"
     "},"))
   ('(link (:type "file" :path "image.png"))
    '(640 . 480)
    (ox-jupyter--concat-multiline
     "base-64-encoded-line"
     "base-64-encoded-line")
    (ox-jupyter--concat-multiline
     "{"
     "  \"output_type\": \"display_data\","
     "  \"data\": {"
     "    \"image/png\": ["
     "      \"base-64-encoded-line\","
     "      \"base-64-encoded-line\""
     "    ]"
     "  },"
     "  \"metadata\": {"
     "    \"image/png\": {"
     "      \"width\": 640,"
     "      \"height\": 480"
     "    }"
     "  }"
     "},")))
  (cl-letf (((symbol-function 'insert-file-contents-literally) #'ignore)
            ((symbol-function 'base64-encode-region)
             (lambda (&rest _) (insert encoded-data)))
            ((symbol-function 'image-size)
             (lambda (&rest _) image-size)))
    (should (equal (ox-jupyter--link link nil nil)
                   expected-text))))

(ert-deftest-parametrize ox-jupyter-paragraph
  (paragraph contents expected-text)
  (('(paragraph (:parent (section nil nil))) "section paragraph"
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"section paragraph\""
     "  ]"
     "},"))
   ('(paragraph (:parent (section nil nil))) "no trailing newlines\n"
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"no trailing newlines\""
     "  ]"
     "},"))
   ('(paragraph (:parent (section nil nil))) "keep interior\nnewlines\n"
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"keep interior\\n\","
     "    \"newlines\""
     "  ]"
     "},"))
   ('(paragraph (:parent (item nil nil))) "list item paragraph\n"
    "list item paragraph\n"))
  (should (equal (ox-jupyter--paragraph paragraph contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-plain-list
  (plain-list contents expected-text)
  (('(plain-list (:type unordered)) "- a\n- plain\n- list\n"
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"- a\\n\","
     "    \"- plain\\n\","
     "    \"- list\""
     "  ]"
     "},"))
   ('(plain-list (:type unordered)) "- a\n  - nested\n  - plain\n- list\n"
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"- a\\n\","
     "    \"  - nested\\n\","
     "    \"  - plain\\n\","
     "    \"- list\""
     "  ]"
     "},"))
   ('(plain-list (:type unordered :parent (item)))
    "- a\n- plain\n- list\n"
    "  - a\n  - plain\n  - list\n")
   ('(plain-list (:type unordered :parent (item)))
    "- a\n  - nested\n  - plain\n- list\n"
    "  - a\n    - nested\n    - plain\n  - list\n"))
  (should (equal (ox-jupyter--plain-list plain-list contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-src-block
  (src-block expected-text)
  (('(src-block (:language "jupyter" :value "some code\n"))
    (ox-jupyter--concat-multiline
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
    (ox-jupyter--concat-multiline
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
    (ox-jupyter--concat-multiline
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
