
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
        (ref-str (multiply-string "abcdefghijklmnopqrstuvwxyz" 100))
        (mod-id (module-last-loaded-id)))

    (should (> mod-id 0))
    (should (= (module-global-refs-count mod-id) 1))
    (should (string= ref-str mod-str))
    (should (eq mod-str (car (gethash mod-id module-refs-hash))))))
