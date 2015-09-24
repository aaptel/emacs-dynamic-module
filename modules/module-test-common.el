(defun module-path (current-file)
  (let* ((current-dir (file-name-directory current-file))
         (basename (file-name-nondirectory (directory-file-name current-dir)))
         (module (concat current-dir basename ".so")))
    module))

(defun module-global-refs-count (&optional id)
  (let ((count 0))
    (if id
        (setq count (length (gethash id module-refs-hash)))
      (maphash (lambda (k v)
                 (setq count (+ count (length v))))
               module-refs-hash))
    count))

(defun module-last-loaded-id ()
  "Return the id of the last module loaded that made global values.
Doesn't work if the last module didnt make global values"
  (let ((max-id 0))
    (maphash (lambda (k v)
               (when (> k max-id)
                 (setq max-id k)))
             module-refs-hash)
    max-id))

(provide 'module-test-common)
