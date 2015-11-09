
(require 'ert)
(require 'module-test-common)

;; #$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$ (expand-file-name (buffer-file-name)))))

(defun multiply-string (s n)
  (let ((res ""))
    (dotimes (i n res)
      (setq res (concat res s)))))

(ert-deftest modt-globref-make-test ()
  (let ((mod-str (modt-globref-make))
        (ref-str (multiply-string "abcdefghijklmnopqrstuvwxyz" 100)))
    (garbage-collect) ;; XXX: not enough to really test but it's something..
    (should (string= ref-str mod-str))))
