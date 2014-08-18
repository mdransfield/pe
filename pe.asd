;;;; pe.asd

(asdf:defsystem #:pe
  :serial t
  :description "Solutions to project euler problems"
  :author "Martin Dransfield <mdransfield@gmail.com>"
  :license "GPL v2 see LICENSE"
  :depends-on (#:primes
               #:fibonacci)
  :components ((:file "package")))

