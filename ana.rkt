#lang racket
(require rackunit rackunit/text-ui)
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

(define word-list empty)

; For each line in file, append to list
; Return list
; (credit: http://rosettacode.org/wiki/Read_a_file_line_by_line)
(define (read-next-line-iter file lst)
  (let ([line (read-line file)])
    (unless (eof-object? line)         
      (read-next-line-iter file (cons lst line))))
  lst)

(define ;(find-anagrams word)
  (fa word) ; test name
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
  (define f (open-input-file "/usr/share/dict/words"))
  (read-next-line-iter f empty)
  ;(display my-word-list)
  ;(let ([my-hash (hash-words my-word-list)])
  ;  (hash-ref my-hash word (printf "Not found: ~a\n" word))
  )

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
                                "aign"  '("gian" "naig" "gnia")
                                )
                          ]
           [result-hash (hash-words '("gel" "leg" "gnia" "gian" "naig"))]
           [exp-values (hash-ref expected-hash "egl")]
           [res-values (hash-ref result-hash "egl")])
      (check-equal? (sort exp-values string<?) 
                    (sort res-values string<?))))
   
   (test-case
    "Read lines"
    (let* ([file (open-input-file "10-lines.txt")]
           [exp-list '("A" "A's" "AA's" "AB's" "ABM's" "AC's" 
                           "ACTH's" "AI's" "AIDS's" "AM's")]
           [my-lst empty]
           [retrieved-list (read-next-line-iter file my-lst)])
      (check-equal? retrieved-list exp-list))
    )
   ))

(run-tests sortw-tests)
(run-tests anagram-tests)