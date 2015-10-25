
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$ (expand-file-name (buffer-file-name)))))

(ert-deftest modt-error-signal-test ()
  (should-error (modt-error-signal)))

(ert-deftest modt-error-throw-test ()
  (should (equal
           (catch 'tag
             (modt-error-throw)
             (ert-fail "expected throw"))
           65)))

(ert-deftest modt-error-funcall-normal ()
  (should (equal (modt-error-funcall (lambda () 23))
                 23)))

(ert-deftest modt-error-funcall-signal ()
  (should (equal (modt-error-funcall (lambda () (signal 'error '(32))))
                 '(signal error (32)))))

(ert-deftest modt-error-funcall-throw ()
  (should (equal (modt-error-funcall (lambda () (throw 'tag 32)))
                 '(throw tag 32))))
