;;; upbo-tests.el --- tests for Upbo
;; Copyright (C) 2017 Sungho Kim (shiren)
;; License: MIT
;;; Commentary:
;;; Code:

(require 'ert)

(require 'upbo)

(ert-deftest addition-test ()
       (should (= (+ 1 2) 3)))

(provide 'upbo-test)
;;; upbo-test.el ends here
