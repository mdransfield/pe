;;;; pe.asd

(asdf:defsystem #:pe
  :serial t
  :description "Solutions to project euler problems"
  :author "Martin Dransfield <mdransfield@gmail.com>"
  :license "GPL v2 see LICENSE"
  :depends-on (#:primes
               #:fibonacci)
  :components ((:file "package")
	       (:file "001") (:file "002") (:file "003") (:file "004") (:file "005")
	       (:file "006") (:file "007") (:file "008") (:file "009") (:file "010")
	       (:file "011") (:file "012") (:file "013") (:file "014") (:file "015")
	       (:file "016") (:file "017") (:file "018") (:file "019") (:file "020")
	       (:file "021") (:file "022") (:file "023") (:file "024") (:file "025")
	       (:file "026") (:file "027") (:file "028") (:file "029") (:file "030")
	       (:file "031")))

