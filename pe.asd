;;;; pe.asd

(asdf:defsystem #:pe
  :serial t
  :description "Solutions to project euler problems"
  :author "Martin Dransfield <mdransfield@gmail.com>"
  :license "GPL v2 see LICENSE"
  :depends-on (#:primes
               #:fibonacci)
  :components ((:file "package")
	       (:file "001")
	       (:file "002")
	       (:file "003")
	       (:file "004")
	       (:file "005")
	       (:file "006")
	       (:file "007")
	       (:file "008")
	       (:file "009")
	       (:file "010")
	       (:file "011")))

