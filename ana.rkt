#lang racket

(provide find-anagrams
         string-sort-letters
         hash-words)

;; (require readline) ; TODO for prompting users

;; Given a word, return its anagram where all letters are sorted
(define (string-sort-letters word)
  (list->string (sort (string->list word) char<?)))

;; Given a list, build a hash where key is the sorted anagram, 
;; and value is a list of anagrams
;; FIXME: dupes get duplicated in the has values! do we want that? (I guess we do)
(define (hash-words input-lst)
  (define h (make-hash))
  (for ([w (in-list input-lst)])
    (define k (string-sort-letters w))
    (hash-set! h k 
               (cons w (hash-ref h k empty))))
  h)

(define (find-anagrams w)
  ;; - Open dictionary file (/usr/share/dict/words), 
  ;; - read the entries in a list, and 
  ;; - build a hash table where the key is the sorted letters, and the value is a
  ;;   list of words with those letters.
  ;;
  ;; Complexity: O(N), where N is the number of words in the dictionary
  ;;
  ;; Return value: the list of words given the key
  ;;
  ;; Improvement: read the dict once at program startup, and stay open until the program is
  ;; explicitly closed, allowing the query of several words.
  
  (define words (file->lines "/usr/share/dict/words"))  
  (define h (hash-words words))
  (hash-ref h (string-sort-letters w) (lambda () (printf "Not found: ~a\n" w))))
