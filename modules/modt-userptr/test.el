
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$ (expand-file-name (buffer-file-name)))))

(ert-deftest modt-userptr-fun-test ()
  (let* ((n 42)
         (v (modt-userptr-make n))
         (r (modt-userptr-get v)))

    (should (eq (type-of v) 'user-ptr))
    (should (integerp r))
    (should (= r n))))

;; TODO: try to test finalizer
