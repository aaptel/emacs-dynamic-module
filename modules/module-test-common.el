
(defun module-path (current-file)
  (let* ((current-dir (file-name-directory current-file))
         (basename (file-name-nondirectory (directory-file-name current-dir)))
         (module (concat current-dir basename ".so")))
    module))

(provide 'module-test-common)
