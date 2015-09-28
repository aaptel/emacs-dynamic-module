
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$ (expand-file-name (buffer-file-name)))))

(ert-deftest modt-error-signal-test ()
  (should-error (modt-error-signal)))

(ert-deftest modt-error-funcall-test ()
  (should (equal (modt-error-funcall)
                 '(signal error nil))))
