(require 'ert)
(require 'opaque)

(ert-deftest opaque-value ()
  "Tests creation/access/release of opaque objects"
  (dotimes (i 500)
    (let ((h))
      (setq h (opaque-make 4 5 6))
      (should (= (opaque-get h 'a) 4))
      (should (= (opaque-get h 'b) 5))
      (should (= (opaque-get h 'c) 6))
      (opaque-free h))))
