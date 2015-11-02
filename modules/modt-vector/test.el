
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$ (expand-file-name (buffer-file-name)))))

(ert-deftest modt-vector-test ()
  (dolist (s '(2 10 100 1000))
    (dolist (e '(42 foo "foo"))
      (let* ((v-ref (make-vector 2 e))
             (eq-ref (eq (aref v-ref 0) (aref v-ref 1)))
             (v-test (make-vector s nil)))
        
        (should (eq (modt-vector-fill v-test e) t))
        (should (eq (modt-vector-eq v-test e) eq-ref))))))


        
        
             


