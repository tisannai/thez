#!chezscheme
(library (tuile compatible)
  (export comp:eval
          comp:hash-make
          comp:hash-ref
          comp:hash-set!
          comp:hash-contains?
          comp:hash-remove!
          comp:hash-keys
          comp:hash-values
          comp:substring
          comp:datum->string
          comp:error
          comp:dir-list
          comp:command-line
          comp:sort
          comp:with-output-to-file
          )
  (import
    (chezscheme))

  ;;  (define (comp:1+ v) (+ v 1))
  ;;  (define (comp:1+ v) (1+ v))

  (define (comp:eval datum)
    (eval datum (interaction-environment)))

  ;; Make hash table with define key-type: symbol, string.
  (define (comp:hash-make key-type)
    (case key-type
      ((symbol) (make-hashtable symbol-hash symbol=?))
      ((string) (make-hashtable string-hash string=?))))

  (define (comp:hash-ref hsh key) (hashtable-ref hsh key #f))

  (define comp:hash-set! hashtable-set!)

  (define comp:hash-contains? hashtable-contains?)

  (define comp:hash-remove! hashtable-delete!)

  (define (comp:hash-keys hsh) (vector->list (hashtable-keys hsh)))

  (define (comp:hash-values hsh)
    (let-values (((keys values) (hashtable-entries hsh)))
      (vector->list values)))

  (define (comp:substring str i1 . rest)
    (if (pair? rest)
        (substring str i1 (car rest))
        (substring str i1 (string-length str))))

  (define (comp:datum->string datum)
    (with-output-to-string (lambda ()
                             (write datum))))

  (define (comp:error msg)
    (error #f msg #f))

  (define comp:dir-list directory-list)

  (define comp:command-line command-line-arguments)
  
  (define (comp:sort lst pred) (list-sort pred lst))

  (define (comp:with-output-to-file filename proc)
    (when (file-exists? filename)
      (delete-file filename))
    (with-output-to-file filename proc))

  )
