(require 'ert)

;; basic module test should go here

(ert-deftest fmod-require ()
  "Tests bindings after require"
  (skip-unless (not (fboundp 'fmod)))
  (require 'fmod)
  (should (fboundp 'fmod)))

(ert-deftest fmod-doc ()
  "Tests docstring"
  ;; core functions docstrings should work
  (should (string= (documentation 'base64-decode-string 'raw) "Base64-decode STRING and return the result."))

  (require 'fmod)

  ;; even after a module was added
  (should (string= (documentation 'base64-decode-string 'raw) "Base64-decode STRING and return the result.

(fn STRING)"))
  ;; check new function doc
  (should (string= (documentation 'fmod 'raw) "Returns the floating-point remainder of NUMER/DENOM

(fn NUMER DENOM)")))

(ert-deftest fmod-value ()
  "Tests fmod calls"
  (require 'fmod)
  (should (= (fmod 3 2) 1))
  ;; XXX: edge cases in man fmod(3)
  ;; (should (= (fmod 3 2) NaN))
  ;; (should (= (fmod inf 1) NaN))
  ;; (should (= (fmod 3 0) NaN))
  )
