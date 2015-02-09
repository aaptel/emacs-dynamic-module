(require 'ert)
(require 'cl)
(require 'yaml)

(ert-deftest yaml-scalar ()
  "Tests scalar parsing"
  (should (yaml-equal '("abc")
                      (yaml-parse-string
                       "---
abc
"))))

(ert-deftest yaml-sequence ()
  "Tests sequence parsing"
  (should (yaml-equal '(("abc" "def" "ghi" "jkl"))
                      (yaml-parse-string
                       "---
- abc
- def
- ghi
- jkl
"))))

(ert-deftest yaml-map ()
  "Tests map parsing"
  (should (yaml-equal '(#s(hash-table data ("a" "1"
                                            "b" "2"
                                            "c" "3")))
                      (yaml-parse-string
                       "---
a: 1
b: 2
c: 3
"))))

(ert-deftest yaml-multi-doc ()
  "Tests documents parsing"
  (should (yaml-equal
           '(#s(hash-table data ("a" "1"
                                 "b" ("1" "2" "3")))
               #s(hash-table data ("foo" #s(hash-table data ("bar" "1"
                                                             "baz" "2"
                                                             "bad" "3"))
                                   "zob" ("42" "43")))
               "abc")
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
  (should (yaml-equal
           '(#s(hash-table
                data
                ("invoice" "34843"
                 "date" "2001-01-23"
                 "bill-to"
                 #s(hash-table
                    data
                    ("given" "Chris"
                     "family" "Dumars"
                     "address"
                     #s(hash-table
                        data
                        ("lines" "458 Walkman Dr.\nSuite #292\n"
                         "city" "Royal Oak"
                         "state" "MI"
                         "postal" "48046"))))
                 "ship-to" #s(hash-table
                              data
                              ("given" "Chris"
                               "family" "Dumars"
                               "address"
                               #s(hash-table
                                  data
                                  ("lines" "458 Walkman Dr.\nSuite #292\n"
                                   "city" "Royal Oak"
                                   "state" "MI"
                                   "postal" "48046")))))))
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

(defun yaml-equal (a b)
  "Basic, slow, deep equal that supports hash-tables. No circular stuff, no optimizations, nothing."
  (and
   (eq (type-of a) (type-of b))
   (or
    (eq a b)
    (and (number-or-marker-p a) (= a b))
    (and (stringp a) (string= a b))
    (and (sequencep a)
         (let ((la (length a))
               (lb (length b))
               (res t))
           (and (= la lb)
                (catch 'ret
                  (dotimes (i la t)
                    (when (null (yaml-equal (elt a i) (elt b i)))
                      (throw 'ret nil)))))))
    (and (hash-table-p a)
         (let ((h1 a)
               (h2 b))
           (and (hash-table-p h1) (hash-table-p h2)
                (equal (hash-table-count h1) (hash-table-count h2))
                ;;(equal (hash-table-weakness h1) (hash-table-weakness h2))
                ;;(equal (hash-table-rehash-threshold h1) (hash-table-rehash-threshold h2))
                (loop as notfound = (gensym)
                      for k being the hash-keys of h1
                      as v1 = (gethash k h1 notfound)
                      as v2 = (gethash k h2 notfound)
                      if (or (eq h1 notfound) (eq h2 notfound) (not (yaml-equal v1 v2)))
                      return nil
                      finally return t)))))))
