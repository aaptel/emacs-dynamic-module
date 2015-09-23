
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$ (expand-file-name (buffer-file-name)))))

(ert-deftest modt-string-a-to-b-test ()
  (should (string= (modt-string-a-to-b "aaa") "bbb")))
