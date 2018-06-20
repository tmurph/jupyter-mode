;;; ox-jupyter-test --- Unit tests for the Jupyter library

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

;;; Unit tests

;; Parsing functions

(ert-deftest-parametrize ox-jupyter-lob-babel-call
  (babel-call lob-alist expected-text)
  (('(babel-call (:call "setup_code"))
    '((setup_code :ignore "code snippet"
                  :ignore :ignore :ignore :ignore :ignore))
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"code\","
     "  \"execution_count\": null,"
     "  \"metadata\": {"
     "  },"
     "  \"outputs\": [],"
     "  \"source\": ["
     "    \"code snippet\""
     "  ]"
     "},")))
  (let ((org-babel-library-of-babel lob-alist))
    (should (equal (ox-jupyter--babel-call babel-call nil nil)
                   expected-text))))

(ert-deftest-parametrize ox-jupyter-buffer-babel-call
  (babel-call buffer-contents expected-text)
  (('(babel-call (:call "setup_code"))
    (ox-jupyter--concat-multiline
     "#+NAME: setup_code"
     "#+BEGIN_SRC jupyter"
     "  code snippet"
     "#+END_SRC")
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"code\","
     "  \"execution_count\": null,"
     "  \"metadata\": {"
     "  },"
     "  \"outputs\": [],"
     "  \"source\": ["
     "    \"code snippet\""
     "  ]"
     "},")))
  (with-temp-buffer
    (insert buffer-contents)
    (should (equal (ox-jupyter--babel-call babel-call nil nil)
                   expected-text))))

(ert-deftest-parametrize ox-jupyter-example-block
  (example-block expected-text)
  (('(example-block (:value "    some text" :parent (section)))
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"    some text\""
     "  ]"
     "},"))
   ('(example-block (:value "some text" :parent (src-block)))
    (ox-jupyter--concat-multiline
     "["
     "  {"
     "    \"name\": \"stdout\","
     "    \"output_type\": \"stream\","
     "    \"text\": ["
     "      \"some text\""
     "    ]"
     "  }"
     "],")))
  (should (equal (ox-jupyter--example-block example-block nil nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-fixed-width
  (fixed-width expected-text)
  (('(fixed-width (:value "some text" :parent (section)))
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"some text\""
     "  ]"
     "},"))
   ('(fixed-width (:value "some text" :parent (src-block)))
    (ox-jupyter--concat-multiline
     "["
     "  {"
     "    \"name\": \"stdout\","
     "    \"output_type\": \"stream\","
     "    \"text\": ["
     "      \"some text\""
     "    ]"
     "  }"
     "],")))
  (should (equal (ox-jupyter--fixed-width fixed-width nil nil)
                 expected-text)))

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
     "},"))
   ('(headline (:raw-value "Link to me" :level 1 :CUSTOM_ID "unique"))
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"<a id='unique'></a>\""
     "  ]"
     "},"
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     "    \"# Link to me\""
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
   ;; Just FYI, the default Org Element parser creates inline link
   ;; objects that always match the following lineage pattern in the
   ;; parse tree.  However, to distinguish links that are the result of
   ;; source blocks, the ox-jupyter library includes tree-modifying code
   ;; the makes the link the grandchild of a source block instead of a
   ;; section.  See `ox-jupyter--merge-code-results' and
   ;; `ox-jupyter--adopt-next-paragraph-maybe'.  See the
   ;; ox-jupyter-image-link tests to see how those links are handled.
   ('(link (:type "file" :path "image.png"
                  :parent (paragraph (:parent (section)))))
    "description"
    "![description](image.png)")
   ('(link (:type "fuzzy" :path "*headline")) "description"
    "[description](#headline)")
   ('(link (:type "custom-id" :raw-link "#headline")) "description"
    "[description](#headline)"))
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

(ert-deftest-parametrize ox-jupyter-line-break
  (line-break contents expected-text)
  (('(line-break) nil "  \n")
   ('(line-break) "next paragraph" "  \nnext paragraph"))
  (should (equal (ox-jupyter--line-break line-break contents nil)
                 expected-text)))

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
    "list item paragraph\n")
   ('(paragraph (:parent (src-block nil nil)))
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
     "      \"width\": 640,"
     "      \"height\": 480"
     "    }"
     "  }"
     "},")
    (ox-jupyter--concat-multiline
     "["
     "  {"
     "    \"output_type\": \"display_data\","
     "    \"data\": {"
     "      \"image/png\": ["
     "        \"base-64-encoded-png-data\""
     "      ]"
     "    },"
     "    \"metadata\": {"
     "      \"image/png\": {"
     "        \"width\": 640,"
     "        \"height\": 480"
     "      }"
     "    }"
     "  }"
     "],"))
   ('(paragraph (:parent (section nil nil)))
    (concat "break up really long lines"
            (make-string (- ox-jupyter--source-line-max
                            (length "break up really long lines"))
                         ?-)
            "at "
            (format "%s" ox-jupyter--source-line-max)
            " characters wide")
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"markdown\","
     "  \"metadata\": {"
     "  },"
     "  \"source\": ["
     (concat "    \""
             "break up really long lines"
             (make-string (- ox-jupyter--source-line-max
                             (length "break up really long lines"))
                          ?-)
             "\",")
     (concat "    \""
             "at "
             (format "%s" ox-jupyter--source-line-max)
             " characters wide\"")
     "  ]"
     "},"))
   ('(paragraph (:parent (line-break nil nil))) "post line break\n"
    "post line break\n"))
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
    "  - a\n    - nested\n    - plain\n  - list\n")
   ('(plain-list (:type unordered :parent (line-break)))
    "- no changes"
    "- no changes"))
  (should (equal (ox-jupyter--plain-list plain-list contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-src-block
  (src-block contents expected-text)
  (('(src-block (:language "jupyter" :value "some code")) nil
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"code\","
     "  \"execution_count\": null,"
     "  \"metadata\": {"
     "  },"
     "  \"outputs\": [],"
     "  \"source\": ["
     "    \"some code\""
     "  ]"
     "},"))
   ('(src-block (:value "code\nthat spans\nmulti lines")) nil
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
   ('(src-block (:value "  code\n  that comes\n  indented")) nil
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
     "},"))
   ('(src-block (:value "no\ntrailing\nnewlines\n")) nil
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"code\","
     "  \"execution_count\": null,"
     "  \"metadata\": {"
     "  },"
     "  \"outputs\": [],"
     "  \"source\": ["
     "    \"no\\n\","
     "    \"trailing\\n\","
     "    \"newlines\""
     "  ]"
     "},"))
   ('(src-block (:value "code"))
    (ox-jupyter--concat-multiline
     "["
     "  {"
     "    \"output_type\": \"display_data\","
     "    \"data\": {"
     "      \"image/png\": [\"base-64-encoded-png-data\"]"
     "    },"
     "    \"metadata\": {"
     "      \"image/png\": {"
     "        \"width\": 640,"
     "        \"height\": 480"
     "      }"
     "    }"
     "  }"
     "]")
    (ox-jupyter--concat-multiline
     "{"
     "  \"cell_type\": \"code\","
     "  \"execution_count\": null,"
     "  \"metadata\": {"
     "  },"
     "  \"outputs\": ["
     "    {"
     "      \"output_type\": \"display_data\","
     "      \"data\": {"
     "        \"image/png\": ["
     "          \"base-64-encoded-png-data\""
     "        ]"
     "      },"
     "      \"metadata\": {"
     "        \"image/png\": {"
     "          \"width\": 640,"
     "          \"height\": 480"
     "        }"
     "      }"
     "    }"
     "  ],"
     "  \"source\": ["
     "    \"code\""
     "  ]"
     "},")))
  (should (equal (ox-jupyter--src-block src-block contents nil)
                 expected-text)))

(ert-deftest-parametrize ox-jupyter-template
  (contents info version expected-text)
  (((ox-jupyter--concat-multiline
     "["
     "  {"
     "    \"cell_type\": \"example1\""
     "  },"
     "  {"
     "    \"cell_type\": \"example2\""
     "  }"
     "]")
    '(:jupyter-metadata "") "4.0"
    (ox-jupyter--concat-multiline
     "{"
     "  \"cells\": ["
     "    {"
     "      \"cell_type\": \"example1\""
     "    },"
     "    {"
     "      \"cell_type\": \"example2\""
     "    }"
     "  ],"
     "  \"metadata\": {"
     "  },"
     "  \"nbformat\": 4,"
     "  \"nbformat_minor\": 0"
     "},"))
   ((ox-jupyter--concat-multiline
     "["
     "  {"
     "    \"cell_type\": \"example\""
     "  }"
     "]")
    '(:jupyter-metadata "something") "4.0"
    (ox-jupyter--concat-multiline
     "{"
     "  \"cells\": ["
     "    {"
     "      \"cell_type\": \"example\""
     "    }"
     "  ],"
     "  \"metadata\": \"something\","
     "  \"nbformat\": 4,"
     "  \"nbformat_minor\": 0"
     "},")))
  (let ((ox-jupyter--nbformat version))
    (should (equal (ox-jupyter--template contents info)
                   expected-text))))

;; Structure functions

(ert-deftest-parametrize ox-jupyter-merge-after-line-break
  (initial-structure expected-structure)
  (('(#0=(section
          nil
          #1=(paragraph
              (:parent #0#)
              "first paragraph contents"
              (line-break (:parent #1#)))
          #2=(paragraph
              (:parent #0#)
              "second paragraph contents")))
    '(#3=(section
          nil
          #4=(paragraph
              (:parent #3#)
              "first paragraph contents"
              #5=(line-break
                  (:parent #4#)
                  #6=(paragraph
                      (:parent #5#)
                      "second paragraph contents"))))))
   ('(#7=(section
          nil
          #8=(paragraph
              (:parent #7#)
              "first paragraph contents"
              (line-break (:parent #8#)))
          #9=(plain-list
              (:parent #7#))))
    '(#10=(section
           nil
           #11=(paragraph
                (:parent #10#)
                "first paragraph contents"
                #12=(line-break
                     (:parent #11#)
                     #13=(plain-list
                          (:parent #12#))))))))
  (should (equal (ox-jupyter--merge-after-line-break
                  initial-structure nil nil)
                 expected-structure)))

(provide 'ox-jupyter-test)
;;; ox-jupyter-test.el ends here
