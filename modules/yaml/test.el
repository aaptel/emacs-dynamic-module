(require 'ert)
(require 'yaml)

(ert-deftest yaml-scalar ()
  "Tests scalar parsing"
  (should (equal '(("abc"))
                 (yaml-parse-string
                  "---
abc
"))))

(ert-deftest yaml-sequence ()
  "Tests sequence parsing"
  (should (equal '(("abc" "def" "ghi" "jkl"))
                 (yaml-parse-string
                  "---
- abc
- def
- ghi
- jkl
"))))

;; TODO: need deep equal for hash-tables for the rest

;; TODO
(ert-deftest yaml-map ()
  "Tests map parsing"
  (skip-unless nil)
  (should (equal 'TODO
                 (yaml-parse-string
                  "---
a: 1
b: 2
c: 3
"))))

;; TODO
(ert-deftest yaml-multi-doc
    "Tests documents parsing"
  (should (equal 'TODO
                 (yaml-parse-string
                  "---
a: 1
b:
    - 1
    - 2
    - 3
---
foo:
    bar: 1
    baz: 2
    bad: 3
zob:
    - 42
    - 43
---
abc
"))))

(ert-deftest yaml-alias ()
  "Tests alias parsing"
  (should (equal 'TODO
                 (yaml-parse-string
                  "---
invoice: 34843
date   : 2001-01-23
bill-to: &id001
    given  : Chris
    family : Dumars
    address:
        lines: |
            458 Walkman Dr.
            Suite #292
        city    : Royal Oak
        state   : MI
        postal  : 48046
ship-to: *id001
"))))
