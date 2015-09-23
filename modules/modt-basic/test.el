
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$ (expand-file-name (buffer-file-name)))))

(ert-deftest modt-basic-sum-test ()
  (should (= (modt-basic-sum 1 2) 3)))
