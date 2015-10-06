#lang racket

(require "ana.rkt")
(require rackunit rackunit/text-ui)

(module+ test
  
  
  ;(require 2htdp/batch-io)
  
  (define sortw-tests
    (test-suite
     "sortw suite"
     
     (test-equal? "Empty string"              
                  (sortw "") "")
     
     (test-equal? "leg -> egl"              
                  (sortw "leg") "egl")
     (test-case 
      "Long word with repeated letters"
      (check-equal? (sortw "heautontimoroumenos") "aeehimmnnoooorsttuu")
      (check-equal? 
       (string-length "heautontimoroumenos")
       (string-length "aeehimmnnoooorsttuu")))   
     ))
  
  (define anagram-tests
    (test-suite
     "anagram suite"
     
     (test-case 
      "Test list hasher"
      (let* ([expected-hash (hash "egl"  '("gel" "leg")
                                  "aign"  '("gian" "naig" "gnia"))]
             [result-hash (hash-words '("gel" "leg" "gnia" "gian" "naig"))]
             [exp-values (hash-ref expected-hash "egl")]
             [res-values (hash-ref result-hash "egl")])
        (check-equal? (sort exp-values string<?) 
                      (sort res-values string<?))))
     
     #|
   (test-case
    "Read lines"
    (let* ([file (open-input-file "10-lines.txt")]
           [exp-list '("A" "A's" "AA's" "AB's" "ABM's" "AC's" 
                           "ACTH's" "AI's" "AIDS's" "AM's")]           
           [retrieved-list (read-next-line-iter file)])
      (check-equal? retrieved-list exp-list))
    )
   |#
     
     (test-case
      "Test anagram finder"
      (let ([an (find-anagrams "live")]
            [an2 (find-anagrams "revenue")]
            [an3 (find-anagrams "ixxa")])
        (check-equal? an '("vile" "veil" "live" "evil"))
        (check-equal? an2 '("revenue"))
        (check-equal? an3 (void)) ; Also prints "Not found: ixxa"
        ))))  
  
(run-tests sortw-tests)
(run-tests anagram-tests))