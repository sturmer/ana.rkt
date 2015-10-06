#lang racket
(require rackunit rackunit/text-ui)
(require racket/string)
(require 2htdp/batch-io)

;(require readline) ; for prompting users

; Given a word, return its anagram where all letters are sorted
(define (sortw word)
  (list->string (sort (string->list word) char<?)))

; Given a list, build a hash where key is the sorted anagram, 
; and value is a list of anagrams
; FIXME: dupes get duplicated in the has values! do we want that? (I guess we do)
(define (hash-words input-lst)
  (let ([h (make-hash)])
    (for ([w (in-list input-lst)])
      (let ([k (sortw w)])
        (hash-set! h k 
                   (cons w (hash-ref h k empty)))))                      
    h)  
  )

; For each line in file, append to list
; Return list
; (credit: http://rosettacode.org/wiki/Read_a_file_line_by_line)
#|
(define (read-next-line-iter file)
  (let ([line (read-line file)])
    (unless (eof-object? line)
      (printf "Line: '~a'\n" line)
      (append (read-next-line-iter file) line))      
    )
  )
|#

(define (fa w)
  #|
- Open dictionary file (/usr/share/dict/words), 
- read the entries in a list, and 
- build a hash table where the key is the sorted letters, and the value is a
   list of words with those letters.

Complexity: O(N), where N is the number of words in the dictionary

Return value: the list of words given the key

Improvement: read the dict once at program startup, and stay open until the program is
explicitly closed, allowing the query of several words.
  |#  
  (define words (read-lines "/usr/share/dict/words"))  
  (let ([h (hash-words words)])
    (hash-ref h (sortw w) (lambda () (printf "Not found: ~a\n" w)))
    ))

; Alias
(define find-anagrams fa)

;;;;;;;;;;;;;;;;;
#| Test suites |#
;;;;;;;;;;;;;;;;;
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
(run-tests anagram-tests)