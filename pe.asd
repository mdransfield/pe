;;;; pe.asd

(asdf:defsystem #:pe
  :serial t
  :description "Solutions to project euler problems"
  :author "Martin Dransfield <mdransfield@gmail.com>"
  :license "GPL v2 see LICENSE"
  :depends-on (#:primes
               #:fibonacci
	       #:sudoku
	       #:cl-utilities
	       #:computable-reals)
  :components
  ((:file "package")
   (:file "utils")
   (:file "factors")
		 (:file "001") (:file "002") (:file "003") (:file "004") (:file "005") (:file "006") (:file "007") (:file "008") (:file "009")
   (:file "010") (:file "011") (:file "012") (:file "013") (:file "014") (:file "015") (:file "016") (:file "017") (:file "018") (:file "019")
   (:file "020") (:file "021") (:file "022") (:file "023") (:file "024") (:file "025") (:file "026") (:file "027") (:file "028") (:file "029")
   (:file "030") (:file "031") (:file "032") (:file "033") (:file "034") (:file "035") (:file "036") (:file "037") (:file "038") (:file "039")
   (:file "040") (:file "041") (:file "042") (:file "043") (:file "044") (:file "045") (:file "046") (:file "047") (:file "048") (:file "049")
   (:file "050") (:file "051") (:file "052") (:file "053") (:file "054") (:file "055") (:file "056") (:file "057")               (:file "059")
   (:file "060") (:file "061") (:file "062") (:file "063") (:file "064") (:file "065") (:file "066")                             (:file "069")
   (:file "070") 							 (:file "075") (:file "076")               (:file "078")
                 							 (:file "085")               (:file "087")               (:file "089")
                 					   (:file "094") (:file "095") (:file "096") (:file "097") (:file "098")
                 					   (:file "104")
                 	       (:file "112") (:file "113")                                                                       (:file "119")
   (:file "120")			     (:file "123")               (:file "125")
;                                             (:file "134")
                                                                                                                                 (:file "179")
                                                                                                                   (:file "188")
                                                                         (:file "206")
                                                                         (:file "216")
;                               (:file "243")
                               (:file "303")
                                                                                       (:file "357")
                                                                                                                                (:file "700")
   (:file "731")
                                                                                                                   (:file "808")
                                                                         (:file "816")))
