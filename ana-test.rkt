#lang racket

(require "ana.rkt")
(require rackunit rackunit/text-ui)

(module+ test
  (define sortw-tests
    (test-suite
     "sortw suite"
     
     (test-equal? "Empty string"              
                  (string-sort-letters "") "")
     
     (test-equal? "leg -> egl"              
                  (string-sort-letters "leg") "egl")
     (test-case 
      "Long word with repeated letters"
      (check-equal? (string-sort-letters "heautontimoroumenos") "aeehimmnnoooorsttuu")
      (check-equal? 
       (string-length "heautontimoroumenos")
       (string-length "aeehimmnnoooorsttuu")))   
     ))
  
  (define anagram-tests
    (test-suite
     "anagram suite"
     
     (test-case 
      "Test list hasher"
      (define expected-hash (hash "egl"  '("gel" "leg")
                                  "aign"  '("gian" "naig" "gnia")))
      (define result-hash (hash-words '("gel" "leg" "gnia" "gian" "naig")))
      (define exp-values (hash-ref expected-hash "egl"))
      (define res-values (hash-ref result-hash "egl"))
      (check-equal? (sort exp-values string<?) 
                    (sort res-values string<?)))

     (test-case
      "Test anagram finder"
      (define an (find-anagrams "live"))
      (define an2 (find-anagrams "revenue"))
      (define an3 (find-anagrams "ixxa"))
      (check-equal? an '("vile" "veil" "live" "evil"))
      (check-equal? an2 '("revenue"))
      (check-equal? an3 (void))))) ; Also prints "Not found: ixxa"
  
  (run-tests sortw-tests)
  (run-tests anagram-tests))
