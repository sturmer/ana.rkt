#lang racket
(require 2htdp/batch-io)

(provide find-anagrams
         sortw
         hash-words)

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