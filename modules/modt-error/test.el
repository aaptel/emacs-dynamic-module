
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$ (expand-file-name (buffer-file-name)))))

(ert-deftest modt-non-local-exit-signal-test ()
  (should-error (modt-non-local-exit-signal)))

(ert-deftest modt-non-local-exit-throw-test ()
  (should (equal
           (catch 'tag
             (modt-non-local-exit-throw)
             (ert-fail "expected throw"))
           65)))

(ert-deftest modt-non-local-exit-funcall-normal ()
  (should (equal (modt-non-local-exit-funcall (lambda () 23))
                 23)))

(ert-deftest modt-non-local-exit-funcall-signal ()
  (should (equal (modt-non-local-exit-funcall (lambda () (signal 'error '(32))))
                 '(signal error (32)))))

(ert-deftest modt-non-local-exit-funcall-throw ()
  (should (equal (modt-non-local-exit-funcall (lambda () (throw 'tag 32)))
                 '(throw tag 32))))
