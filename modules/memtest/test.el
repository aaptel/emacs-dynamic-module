(require 'ert)
(require 'memtest)

(ert-deftest memtest-basic ()
  "Tests creation/access/release of module objects"
  (let* ((fc (memtest-free-count))
         (n 100))

    (let ((b (memtest-make)))
      (dotimes (i n)
        (should (= (memtest-size b) i))
        (memtest-add b i)
        (should (= (memtest-size b) (1+ i)))))

    ;; force GC
    (garbage-collect)
    (sleep-for 1)
    (garbage-collect)

    (should (= (memtest-free-count) (1+ fc)))))
