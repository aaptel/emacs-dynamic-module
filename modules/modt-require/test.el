
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(defvar current-dir (file-name-directory (or #$ (expand-file-name (buffer-file-name)))))

(ert-deftest modt-require-fun-test ()
  (add-to-list 'load-path current-dir)
  (should (eq (require 'modt-require) 'modt-require))
  (should (eq (modt-require-fun) t)))
