#!chezscheme
(library (tuile utils)
  (export any?
          empty?
          len-0?
          len-1?
          len-2?
          len-3?
          most
          best
          flatten
          flatten-0
          flatten-1
          delete-ref
          list-compact
          clean-list

          pi
          ->integer-fraction
          unspecified
          uns

          ;;          command-line-arguments

          dir-list
          dir-glob
          dir-glob-re
          extname
          expand-file-name

          datum->string
          string->procedure
          common-eval

;;          each-it
          aif
          awhen
          for
          forever
          map-except-last
          map-except-first
          repeat
          repeat-times
          from-to
          from-to-step
          nop

          record-type
          ;;          define-im-record
          ;;          define-fp-record
          ;;          define-mu-record
          ;;
          ;;          define-this-class
          ;;          define-this-method
          ;;          this-ref
          ;;          this-set!
          ;;
          ;;          re-split
          ;;          re-match?
          ;;          re-match
          ;;          re-matches
          ;;          re-sub
          ;;          re-gsub
          ;;
          vector-range
          vector-reverse
          vector-insert
          vector-delete

          assoc-has-key?
          assoc-update!
          assoc-repeat!
          assoc-merge

          hash-has-key?
          hash-keys
          hash-values

          ;;          read-lines-from-port
          ;;          with-each-line-from-port
          ;;          with-each-line-from-file
          ;;          open-output-port-with-filename
          ;;          close-output-port-with-filename
          ;;          with-output-to-filename
          ;;          file->lines
          ;;          lines->file
          ;;          ;;   file->line-list
          ;;          ;;   line-list->file
          ;;
          ;;          capture-shell-command-values
          ;;          capture-shell-command
          ;;          capture-shell-command-stdout
          ;;
          timestamp
          datestamp
          days-in-month

          old-memf
          find-all
          ;;          find-first
          find-member

          ;;          with-exception-terminate
          string-clip
          make-string-list
          sequence
          range
          )
  (import (chezscheme)
          (tuile guile)
          (tuile compatible)
          (tuile re)
          ;;          (only (srfi :152) string-split)
          (only (srfi :1) take drop first second)
          (prefix (srfi :19) srfi:)
          )
  
  
  ;; ------------------------------------------------------------
  ;; Internal functions:


  ;; ------------------------------------------------------------
  ;; External functions:


  (define any?   pair?)
  (define empty? null?)
  (define len-0? null?)
  (define len-1? (lambda (lst) (and (pair? lst) (null? (cdr lst)))))
  (define len-2? (lambda (lst) (and (pair? lst) (pair? (cdr lst)) (null? (cddr lst)))))
  (define len-3? (lambda (lst) (and (pair? lst) (pair? (cdr lst)) (pair? (cddr lst)) (null? (cdddr lst)))))

  ;; Find the item in "lst" that has largest value returned by property
  ;; getter "fn".
  ;;
  ;;     (most identity '(1 2 3))
  ;;
  (define (most fn lst)
    (if (null? lst)
        #f
        (let loop ((tail (cdr lst))
                   (wins (car lst))
                   (max (fn (car lst))))
          (if (pair? tail)
              (let ((score (fn (car tail))))
                (if (> score max)
                    (loop (cdr tail)
                          (car tail)
                          score)
                    (loop (cdr tail)
                          wins
                          max)))
              wins))))

  ;; Find the item in "lst" that is best by property
  ;; comparator "fn".
  ;;
  ;;     (best (lambda (i curwin) (> i curwin)) '(1 2 3))
  ;;
  (define (best fn lst)
    (if (null? lst)
        #f
        (let loop ((tail (cdr lst))
                   (wins (car lst)))
          (if (pair? tail)
              (if (fn (car tail) wins)
                  (loop (cdr tail)
                        (car tail))
                  (loop (cdr tail)
                        wins))
              wins))))


  ;; Flatten (and join) argument list as deep as list goes.
  (define (flatten . rest)
    (let loop ((lst rest)
               (res '()))
      (cond
       ((null? lst) res)
       ((pair? lst) (loop (car lst)
                          (loop (cdr lst)
                                res)))
       (else
        (cons lst res)))))


  ;; Flatten (and join) argument list without collapsing.
  (define (flatten-0 . rest)
    (let loop ((lst rest))
      (if (pair? lst)
          ;; Select proper proc with if.
          ((if (pair? (car lst)) append cons)
           (car lst)
           (loop (cdr lst)))
          '())))


  ;; Flatten (and join) argument list by one level.
  (define (flatten-1 . rest)
    (let loop ((lst rest)
               (res '()))
      (if (pair? lst)
          (cond
           ((pair? (car lst))
            ;; List item.
            (loop (cdr lst)
                  (append res (apply flatten-0 (car lst)))))
           (else
            ;; Non-list item
            (loop (cdr lst)
                  (append res (list (car lst))))))
          res)))

  ;; Delete nth element from list.
  (define (delete-ref lst nth)
    (let loop ((head '())
               (tail lst)
               (i 0))
      (if (and (pair? tail)
               (< i nth))
          (loop (cons (car tail) head)
                (cdr tail)
                (1+ i))
          (append (reverse head)
                  (if (pair? tail) (cdr tail) '())))))


  ;; Compact list by removing (by default) unspecified and false values.
  (define (list-compact lst . opt-pred)
    (let ((pred (if (pair? opt-pred) (car opt-pred) (lambda (item) (not (or (unspecified? item)
                                                                            (not item)))))))
      (filter pred lst)))


  (define (clean-list lst)
    (let filter ((rest lst))
      (if (pair? rest)
          (if (unspecified? (car rest))
              (filter (cdr rest))
              (cons (car rest) (filter (cdr rest))))
          '())))


  ;; PI = 3.141592653589793
  (define pi (* 2 (acos 0)))

  ;; Return integer and fractional parts of real as pair.
  (define (->integer-fraction real decimals)
    (call-with-values (lambda ()
                        (let ((scaler (expt 10 decimals)))
                          (floor/ (* real scaler) scaler)))
      (lambda (q r)
        (cons (inexact->exact (round q))
              (inexact->exact (round r))))))

  (define unspecified (if #f #t))
  (define uns unspecified)


  ;; Return command line arguments, excluding the executable.
  ;;  (define (command-line-arguments)
  ;;    (cdr (command-line)))


  ;; List given directory entries without the dot files.
  (define dir-list comp:dir-list)

  ;; Glob directory.
  ;;
  ;;     (dir-glob "../foo" "*.{c,cc}")
  ;;
  (define (dir-glob dir pat)

    ;; Glob pattern to regexp.
    (define (glob->regexp pat)
      (let ((len (string-length pat))
            (in-selection 0))
        (string-concatenate
         (append
          (list "^")
          (let loop ((i 0))
            (if (< i len)
                (let ((char (string-ref pat i)))
                  (case char
                    ((#\*) (cons "[^.]*" (loop (1+ i))))
                    ((#\?) (cons "[^.]" (loop (1+ i))))
                    ((#\[) (cons "[" (loop (1+ i))))
                    ((#\]) (cons "]" (loop (1+ i))))
                    ((#\{) (begin
                             (set! in-selection (1+ in-selection))
                             (cons "(" (loop (1+ i)))))
                    ((#\}) (begin
                             (set! in-selection (1- in-selection))
                             (cons ")" (loop (1+ i)))))
                    ((#\,) (if (> in-selection 0)
                               (cons "|" (loop (1+ i)))
                               (cons "," (loop (1+ i)))))
                    ((#\\)
                     (cons (list->string (list char (string-ref pat (1+ i))))
                           (loop (+ i 2))))
                    (else
                     (cons (re-quote (make-string 1 char))
                           (loop (1+ i))))))
                '()))
          (list "$")))))

    (dir-glob-re dir (glob->regexp pat)))


  ;; Glob directory with regexp.
  ;;
  ;;     (dir-glob-re "../foo" ".*[.](c|cc)")
  ;;
  (define (dir-glob-re dir pat)
    ;;  (let ((rx (make-regexp pat)))
    ;;    (filter (lambda (x) (regexp-exec rx x)) (dir-list dir)))
    (let ((rx (re-comp pat)))
      (filter (lambda (x) (re-match rx x)) (dir-list dir))))


  ;; Return filename suffix (without the dot).
  (define (extname filename)
    (car (last-pair (string-split filename #\.))))

  ;; Convert filename to absolute, and canonicalize it (as in Emacs).
  (define (expand-file-name filename)
    (if (eq? (string-ref filename 0) #\~)
        (string-append (getenv "HOME") (comp:substring filename 1))
        (canonicalize-path filename)))

  (define datum->string comp:datum->string)

  ;; Convert string to procedure.
  (define (string->procedure str)
    (eval (read (open-input-string str)) (interaction-environment)))

  ;; Eval datum.
  (define (common-eval datum)
    (eval datum (interaction-environment)))

;;  (define-syntax each-it
;;    (lambda (x)
;;      (syntax-case x ()
;;        ((k lst body)
;;         (with-syntax ((it (datum->syntax #'k 'it)))
;;           #'(for-each (lambda (it) body) lst))))))

  ;; Anaphoric if macro.
  ;;
  ;;     (aif (1+ i)
  ;;          it
  ;;          #f)
  ;;  (define-syntax aif
  ;;    (lambda (x)
  ;;      ;; "lst": Convert syntax to datum and convert back to a list of syntax.
  ;;      (let ((lst (map (lambda (i) (datum->syntax x i)) (syntax->datum x))))
  ;;        ;; Create template variable "it".
  ;;        (with-syntax ((it (datum->syntax x 'it)))
  ;;          (display (write #`(let ((it #,(cadr lst)))
  ;;                              (if it #,(caddr lst) #,(cadddr lst)))))
  ;;          (newline)
  ;;          #'#t))))

  (define-syntax aif
    (lambda (x)
      (syntax-case x ()
        ((k test then else)
         (with-syntax ((it (datum->syntax #'k 'it)))
           #'(let ((it test))
               (if it then else)))))))


  ;; Anaphoric when macro.
  ;;
  ;;     (awhen (1+ i)
  ;;          it)
  (define-syntax awhen
    (lambda (x)
      ;; "lst": Convert syntax to datum and convert back to a list of syntax.
      (let ((lst (map (lambda (i) (datum->syntax x i)) (syntax->datum x))))
        ;; Create template variable "it".
        (with-syntax ((it (datum->syntax x 'it)))
          #`(let ((it #,(cadr lst)))
              (when it #,(caddr lst)))))))


  ;; Execute for each in list.
  ;;
  ;;     (for ((i lst))
  ;;       (display i)
  ;;       (newline))
  (define-syntax for
    (lambda (x)
      (syntax-case x ()
        ((for ((i1 l1)) body ...)
         #'(for-each
            (lambda (i1)
              body ...)
            l1))
        ((for ((i1 l1) (i2 l2)) body ...)
         #'(for-each
            (lambda (i1 i2)
              body ...)
            l1 l2)))))


  ;; Loop forever.
  ;;
  ;;     (forever
  ;;      (display "hello")
  ;;      (newline))
  ;;
  (define-syntax forever
    (lambda (x)
      (syntax-case x ()
        ((forever body ...)
         #'(let loop-forever ()
             body ...
             )))))


  ;; Map all list entries except last.
  (define (map-except-last fn lst)
    (reverse (let process ((tail lst)
                           (res '()))
               (cond
                ((null? tail)
                 res)
                ((pair? (cdr tail))
                 (process (cdr tail)
                          (cons (fn (car tail))
                                res)))
                (else
                 (process (cdr tail)
                          (cons (car tail)
                                res)))))))


  ;; Map all list entries except last.
  (define (map-except-first fn lst)
    (reverse (let process ((tail lst)
                           (res '()))
               (cond
                ((null? tail)
                 res)
                ((null? res)
                 (process (cdr tail)
                          (cons (car tail)
                                res)))
                (else
                 (process (cdr tail)
                          (cons (fn (car tail))
                                res)))))))


  (define (fn-pipe arg . chain)
    (let loop ((res arg)
               (chain chain))
      (if (pair? chain)
          (loop ((car chain) res)
                (cdr chain))
          res)))

  (define -> fn-pipe)

  (define (repeat fn cnt)
    (let loop ((i 0))
      (if (< i cnt)
          (cons (fn i) (loop (1+ i)))
          '())))

  (define-syntax repeat-times
    (lambda (x)
      (syntax-case x ()
        ((_ cnt body ...)
         (with-syntax ((i (datum->syntax x 'i)))
           #'(let repeat-loop ((i 0))
               (when (< i cnt)
                 (begin
                   body ...
                   (repeat-loop (1+ i))))))))))

  ;; Run "i" from "from" to "to" with step of 1.
  (define-syntax from-to
    (lambda (x)
      (syntax-case x ()
        ((_ from to body ...)
         (with-syntax ((i (datum->syntax x 'i)))
           #'(let repeat-loop ((i from))
               (when (< i (1+ to))
                 (begin
                   body ...
                   (repeat-loop (1+ i))))))))))

  ;; Run "i" from "from" to "to" with step of "step".
  (define-syntax from-to-step
    (lambda (x)
      (syntax-case x ()
        ((_ from to step body ...)
         (with-syntax ((i (datum->syntax x 'i)))
           #'(let repeat-loop ((i from))
               (when (< i (1+ to))
                 (begin
                   body ...
                   (repeat-loop (+ i step))))))))))


  ;; No operation, i.e. pass argument forward.
  (define (nop arg)
    arg)



  (define (record-type r)
    (if (record? r)
        (record-type-name (record-rtd r))
        #f))

  #|
                                        ; ;
  ;; Create immutable record.           ; ;
  ;;                                    ; ;
  ;; Expand this:                       ; ;
  ;;   (define-im-record foo bar hii)   ; ;
  ;;                                    ; ;
  ;; To this:                           ; ;
  ;;   (define-record-type <foo>        ; ;
  ;;     (make-foo bar hii)             ; ;
  ;;     foo?                           ; ;
  ;;     (bar   foo-bar)                ; ;
  ;;     (hii   foo-hii)                ; ;
  ;;     )                              ; ;
  ;;                                    ; ;
  (define-syntax define-im-record       ; ;
  (lambda (x)                           ; ;
  (let ((stx (syntax->datum x)))        ; ;
  #`(define-record-type #,(datum->syntax x (string->symbol (string-append "<" (symbol->string (cadr stx)) ">"))) ; ;
  (#,(datum->syntax x (string->symbol (string-append "make-" (symbol->string (cadr stx))))) ; ;
  #,@(map (lambda (i) (datum->syntax x i)) (cddr stx))) ; ;
  #,(datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "?"))) ; ;
  #,@(map                               ; ;
  (lambda (i)                           ; ;
  (list                                 ; ;
  (datum->syntax x i)                   ; ;
  (datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "-" (symbol->string i)))))) ; ;
  (cddr stx))))))                       ; ;
                                        ; ;
                                        ; ;
  ;; Create record for functional programming. ; ;
  ;;                                    ; ;
  ;; The setters will modify the record to a record copy with the ; ;
  ;; requested change. This supports functional programming. ; ;
  ;;                                    ; ;
  ;; Expand this:                       ; ;
  ;;   (define-fp-record foo bar hii)   ; ;
  ;;                                    ; ;
  ;; To this:                           ; ;
  ;;   (define-immutable-record-type <foo> ; ;
  ;;     (make-foo bar hii)             ; ;
  ;;     foo?                           ; ;
  ;;     (bar   foo-bar set-foo-bar)    ; ;
  ;;     (hii   foo-hii set-foo-hii)    ; ;
  ;;     )                              ; ;
  ;;                                    ; ;
  (define-syntax define-fp-record       ; ;
  (lambda (x)                           ; ;
  (let ((stx (syntax->datum x)))        ; ;
  #`(define-immutable-record-type #,(datum->syntax x (string->symbol (string-append "<" (symbol->string (cadr stx)) ">"))) ; ;
  (#,(datum->syntax x (string->symbol (string-append "make-" (symbol->string (cadr stx))))) ; ;
  #,@(map (lambda (i) (datum->syntax x i)) (cddr stx))) ; ;
  #,(datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "?"))) ; ;
  #,@(map                               ; ;
  (lambda (i)                           ; ;
  (list                                 ; ;
  (datum->syntax x i)                   ; ;
  (datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) ; ;
  "-"                                   ; ;
  (symbol->string i))))                 ; ;
  (datum->syntax x (string->symbol (string-append "set-" ; ;
  (symbol->string (cadr stx))           ; ;
  "-"                                   ; ;
  (symbol->string i))))))               ; ;
  (cddr stx))))))                       ; ;
                                        ; ;
                                        ; ;
  ;; Create mutable record.             ; ;
  ;;                                    ; ;
  ;; Expand this:                       ; ;
  ;;   (define-mu-record foo bar hii)   ; ;
  ;;                                    ; ;
  ;; To this:                           ; ;
  ;;   (define-record-type <foo>        ; ;
  ;;     (make-foo bar hii)             ; ;
  ;;     foo?                           ; ;
  ;;     (bar   foo-bar set-foo-bar!)   ; ;
  ;;     (hii   foo-hii set-foo-hii!)   ; ;
  ;;     )                              ; ;
  ;;                                    ; ;
  (define-syntax define-mu-record       ; ;
  (lambda (x)                           ; ;
  (let ((stx (syntax->datum x)))        ; ;
  #`(define-record-type #,(datum->syntax x (string->symbol (string-append "<" (symbol->string (cadr stx)) ">"))) ; ;
  (#,(datum->syntax x (string->symbol (string-append "make-" (symbol->string (cadr stx))))) ; ;
  #,@(map (lambda (i) (datum->syntax x i)) (cddr stx))) ; ;
  #,(datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "?"))) ; ;
  #,@(map                               ; ;
  (lambda (i)                           ; ;
  (list                                 ; ;
  (datum->syntax x i)                   ; ;
  (datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) ; ;
  "-"                                   ; ;
  (symbol->string i))))                 ; ;
  (datum->syntax x (string->symbol (string-append "set-" ; ;
  (symbol->string (cadr stx))           ; ;
  "-"                                   ; ;
  (symbol->string i)                    ; ;
  "!")))))                              ; ;
  (cddr stx))))))                       ; ;
                                        ; ;
                                        ; ;
  ;; Define class with members.         ; ;
  ;;                                    ; ;
  ;; Members have initial values and keyword initializers. ; ;
  ;;                                    ; ;
  ;;     (define-this-class <my-class> (<string>) ; ;
  ;;       mem1                         ; ;
  ;;       (mem2 2))                    ; ;
  ;;                                    ; ;
  ;; Becomes:                           ; ;
  ;;                                    ; ;
  ;;      (define-class <my-class> (<string>) ; ;
  ;;        (mem1 #:init-keyword #:mem1 #:init-form #f) ; ;
  ;;        (mem2 #:init-keyword #:mem2 #:init-form 2)) ; ;
  ;;                                    ; ;
  (define-syntax define-this-class      ; ;
  (lambda (x)                           ; ;
  (let* ((stx     (syntax->datum x))    ; ;
  (klass   (cadr stx))                  ; ;
  (bases   (caddr stx))                 ; ;
  (memdefs (cdddr stx))                 ; ;
  (->syn   datum->syntax))              ; ;
  #`(define-class #,(->syn x klass) #,(->syn x bases) ; ;
  #,@(map (lambda (memdef)              ; ;
  (->syn x (let ((mem-name (if (pair? memdef) ; ;
  (car memdef)                          ; ;
  memdef))                              ; ;
  (val (if (pair? memdef)               ; ;
  (cadr memdef)                         ; ;
  #f)))                                 ; ;
  (list mem-name                        ; ;
  #:init-form                           ; ;
  val                                   ; ;
  #:init-keyword                        ; ;
  (string->keyword (symbol->string mem-name)))))) ; ;
  memdefs)))))                          ; ;
                                        ; ;
                                        ; ;
  ;; Define method in a compact form.   ; ;
  ;;                                    ; ;
  ;;     (define-this-method <my-class> (my-method a1 a2 . rest) ; ;
  ;;       body ...)                    ; ;
  ;;                                    ; ;
  ;; Becomes:                           ; ;
  ;;                                    ; ;
  ;;      (define-method (my-method (this <my-class>) a1 a2 . rest) ; ;
  ;;        body ...)                   ; ;
  ;;                                    ; ;
  (define-syntax define-this-method     ; ;
  (lambda (x)                           ; ;
  (let* ((stx   (syntax->datum x))      ; ;
  (klass (cadr stx))                    ; ;
  (metod (caaddr stx))                  ; ;
  (args  (cdaddr stx))                  ; ;
  (body  (cdddr stx))                   ; ;
  (->syn datum->syntax))                ; ;
  #`(define-method #,(if (pair? args)   ; ;
                               ;; Has multiple arguments. ; ;
  (if (cdr (last-pair args))            ; ;
                                   ;; Has ". rest". ; ;
  (append (list (->syn x metod)         ; ;
  (list (->syn x 'this)                 ; ;
  (->syn x klass)))                     ; ;
  (map (lambda (i)                      ; ;
  (->syn x i))                          ; ;
  (drop-right args 1))                  ; ;
  (->syn x (last-pair args)))           ; ;
                                   ;; Fixed list of args. ; ;
  (append (list (->syn x metod)         ; ;
  (list (->syn x 'this)                 ; ;
  (->syn x klass)))                     ; ;
  (->syn x args)))                      ; ;
                               ;; No arguments (or only ". rest"). ; ;
  (if (null? args)                      ; ;
                                   ;; No arguments. ; ;
  (list (->syn x metod)                 ; ;
  (list (->syn x 'this)                 ; ;
  (->syn x klass)))                     ; ;
                                   ;; Only ". rest". ; ;
  (append (list (->syn x metod))        ; ;
  (cons (list (->syn x 'this)           ; ;
  (->syn x klass))                      ; ;
  (->syn x args)))))                    ; ;
  #,@(->syn x body)))))                 ; ;
                                        ; ;
                                        ; ;
  ;; Reference object member.           ; ;
  ;;                                    ; ;
  ;; (this-ref :name)                   ; ;
  ;;   ->                               ; ;
  ;; (slot-ref this ':name)             ; ;
  (define-syntax this-ref               ; ;
  (lambda (x)                           ; ;
  (let* ((stx (syntax->datum x))        ; ;
  (->syn datum->syntax))                ; ;
  #`(slot-ref #,(->syn x 'this) (quote #,(->syn x (cadr stx))))))) ; ;
                                        ; ;
                                        ; ;
  ;; Set object member.                 ; ;
  ;;                                    ; ;
  ;; (this-set! :name value)            ; ;
  ;;   ->                               ; ;
  ;; (slot-set! this ':name value)      ; ;
  (define-syntax this-set!              ; ;
  (lambda (x)                           ; ;
  (let* ((stx (syntax->datum x))        ; ;
             ;;           (->str symbol->string) ; ;
             ;;           (->sym string->symbol) ; ;
  (->syn datum->syntax))                ; ;
  (with-syntax ((this (->syn x 'this))) ; ;
  #`(slot-set! this (quote #,(->syn x (cadr stx))) #,@(->syn x (cddr stx))))))) ; ;
                                        ; ;
  ;; Split string with regexp.          ; ;
  ;;                                    ; ;
  ;; Examples:                          ; ;
  ;;     guile> (re-split "[-x]+" "foo--x--bar---what--") ; ;
  ;;     ("foo" "bar" "what" "")        ; ;
  ;;     guile> (re-split "[-x]+" "foo--x--bar---what--"  'trim) ; ;
  ;;     ("foo" "bar" "what")           ; ;
  ;;     guile> (re-split "[-x]+" "foo--x--bar---what"  'keep) ; ;
  ;;     ("foo" "--x--" "bar" "---" "what") ; ;
  ;;                                    ; ;
  (define (re-split re str . options)   ; ;
  (let ((keep #f) (trim #f))            ; ;
  (when (member 'keep options)          ; ;
  (set! options (delete 'keep options)) ; ;
  (set! keep #t))                       ; ;
  (when (member 'trim options)          ; ;
  (set! options (delete 'trim options)) ; ;
  (set! trim #t))                       ; ;
  (let* ((matches (apply list-matches re str options)) ; ;
  (indices                              ; ;
  (append '(0)                          ; ;
  (fold-right                           ; ;
  (lambda (m acc)                       ; ;
  (cons (match:start m)                 ; ;
  (cons (match:end m) acc))) '()        ; ;
  matches)                              ; ;
  (list (string-length str))))          ; ;
  (substrings                           ; ;
  (pair-fold-right                      ; ;
  (lambda (lst accum)                   ; ;
  (if (or (even? (length lst))          ; ;
  (and keep (> (length lst) 1)))        ; ;
  (cons (apply substring str (take lst 2)) accum) ; ;
  accum))                               ; ;
  '()                                   ; ;
  indices)))                            ; ;
  (if trim                              ; ;
  (reverse! (drop-while                 ; ;
  string-null?                          ; ;
  (reverse! (drop-while string-null? substrings)))) ; ;
  substrings))))                        ; ;
                                        ; ;
                                        ; ;
  ;; Return true if regexp matches str. ; ;
  (define (re-match? re str)            ; ;
  (regexp-match? (regexp-exec (make-regexp re) str))) ; ;
                                        ; ;
  ;; Return regexp match str or false.  ; ;
  (define (re-match re str)             ; ;
  (aif (string-match re str)            ; ;
  (match:substring it)                  ; ;
  #f))                                  ; ;
                                        ; ;
  ;; Return regexp match string list or empty list. ; ;
  (define (re-matches re str)           ; ;
  (map match:substring (list-matches re str))) ; ;
                                        ; ;
  ;; Substitute regexp in string with replacement once. ; ;
  (define (re-sub re str rep)           ; ;
  (aif (string-match re str)            ; ;
  (regexp-substitute #f it 'pre rep 'post) ; ;
  str))                                 ; ;
                                        ; ;
  ;; Substitute regexp in string with replacement globally. ; ;
  (define (re-gsub re str rep)          ; ;
  (aif (string-match re str)            ; ;
  (regexp-substitute/global #f re str 'pre rep 'post) ; ;
  str))                                 ; ;
                                        ; ;
  |#



  ;; Append item to list.
  ;;
  ;;     (append-item! lst 21)
  (define (append-item! lst item)
    (append! lst (list item)))

  ;; Prepend item to list.
  ;;
  ;;     (prepend-item! lst 21)
  (define (prepend-item! lst item)
    (set-cdr! lst (list-copy lst))
    (set-car! lst item))


  (define (list-range lst a b)
    (let ((len (length lst)))
      (if (and (< a len)
               (<= b len))
          (take (drop lst a)
                (- b a))
          '())))

  ;; Get vector elements by range: [a,b).
  ;;
  ;; a is inclusive and b is exclusive.
  ;;
  (define (vector-range vec a b)
    (list->vector (list-range (vector->list vec)
                              a
                              b)))

  ;; Immutable vector reverse.
  (define (vector-reverse vec)
    (list->vector (reverse (vector->list vec))))

  (define (->vector args)
    (cond
     ((vector? args) args)
     ((list? args) (list->vector args))
     (else (vector args))))

  (define (->list args)
    (cond
     ((list? args) args)
     ((vector? args) (vector->list args))
     (else (list args))))

  ;; Insert items to vector position and return a new vector.
  (define (vector-insert vec pos items)
    (let* ((lst (vector->list vec))
           (head (take lst pos))
           (tail (drop lst pos)))
      (list->vector (append head
                            (->list items)
                            tail))))

  ;; Delete count number of items from vector at pos.
  (define (vector-delete vec pos count)
    (let ((lst (vector->list vec)))
      (list->vector (append (take lst pos)
                            (drop lst (+ pos count))))))



  ;; Assoc list has key?
  (define (assoc-has-key? assoc key)
    (member key (map car assoc)))


  ;; Update assoc entry using one argument procedure.
  (define (assoc-update! alist key proc)
    (if (assoc-has-key? alist key)
        (let ((pair (assoc key alist)))
          (set-cdr! pair
                    (proc (cdr pair)))
          alist)
        #f))


  ;; Evaluate "proc" for massoc for each pair in set-list.
  (define (assoc-repeat! alist proc set-list)
    (if (pair? set-list)
        (assoc-repeat! (proc alist
                             (caar set-list)
                             (cdar set-list))
                       proc
                       (cdr set-list))
        alist))


  ;; Combine two mutable assoc lists.
  ;;
  ;;Override and complement entries in A with entries from B.
  (define (assoc-merge a b)
    (define (amerge a b)
      (if (pair? b)
          (amerge (assoc-set! a (caar b) (cdar b))
                  (cdr b))
          a))
    (amerge (amerge (list) a) b))


  ;; Hash table has key?
  (define hash-has-key? comp:hash-contains?)

  ;; Return list of hash table keys.
  (define hash-keys comp:hash-keys)

  ;; Return list of hash table keys.
  (define hash-values comp:hash-values)


  ;; Current time w/o time segment.
  ;;
  ;;     Format (without time): <yymmdd>
  ;;     Format (with    time): <yymmdd_HHMM>
  ;;
  ;; define (timestamp #:key (use-time #f) (use-sec #f))
  (define (timestamp)
    (let* ((use-time #f)
           (use-sec #f)
           (formatter (cond
                       (use-sec
                        "~y~m~d_~H~M~S")
                       (use-time
                        "~y~m~d_~H~M")
                       (else
                        "~y~m~d"))))
      (srfi:date->string (srfi:current-date)
                         formatter)))


  ;; Current date.
  ;;
  ;;     Format: <yyyy-mm-dd>
  ;;
  (define (datestamp)
    (srfi:date->string (srfi:current-date)
                       "~Y-~m-~d"))


  ;; Return the number of days in given month.
  ;;
  (define (days-in-month year month)
    (case month
      ((1 3 5 7 8 10 12)
       31)
      ((4 6 9 11)
       30)
      (else
       ;; February has varying number of days.
       (if (or (= (remainder year 400) 0)
               (and (= (remainder year 4) 0)
                    (not (= (remainder year 100) 0))))
           29
           28))))


  ;; Find all items matched with one argument proc "fn" from "lst".
  (define (old-memf fn lst)
    (let loop ((rest lst)
               (res '()))
      (if (pair? rest)
          (if (fn (car rest))
              (loop (cdr rest)
                    (cons (car rest)
                          res))
              (loop (cdr rest)
                    res))
          (reverse res))))


  ;; Find all items matched with one argument proc "fn" from "lst".
  (define (find-all fn lst)
    (let loop ((tail lst)
               (found< '()))
      (if (pair? tail)
          (if (fn (car tail))
              (loop (cdr tail)
                    (cons (car tail)
                          found<))
              (loop (cdr tail)
                    found<))
          (reverse found<))))


  ;; Find first item matching with one argument proc "fn" from "lst".
  ;;
  ;; Return item or false if no match.
  ;;
  ;; NOTE: Using a delimited continuation, with default-prompt-handler.
  ;;
  ;;  (define (find-first fn lst)
  ;;    (% (let loop ((tail lst))
  ;;         (if (pair? tail)
  ;;             (if (fn (car tail))
  ;;                 (abort (lambda (k)       ; Default prompt handler
  ;;                                        ; requires a proc.
  ;;                          (car tail)))
  ;;                 (loop (cdr tail)))
  ;;             #f))))


  (define (find-member item lst)
    (let ((res (member item lst)))
      (if res (car res) res)))


  ;; Terminate exception (with "handler" of one argument) if exception
  ;; is generated by "proc" (a thunk).
  ;;  (define (with-exception-terminate handler proc)
  ;;    (with-exception-handler handler
  ;;      proc
  ;;      #:unwind? #t))


  ;; Return substring with possibly negative indeces.
  (define (string-clip str . rest)
    (define (normalize index)
      (if (< index 0)
          (+ (+ (string-length str) 1)
             index)
          index))

    (define (order i1 i2)
      (let ((n1 (normalize i1))
            (n2 (normalize i2)))
        (if (< n2 n1)
            (values n2 n1)
            (values n1 n2))))

    (cond
     ((= 0 (length rest))
      str)
     ((= 1 (length rest))
      (substring str
                 0
                 (normalize (first rest))))
     (else
      (call-with-values (lambda () (order (first rest) (second rest)))
        (lambda (n1 n2)
          (substring str
                     n1
                     n2))))))

  ;; Create list of string from symbols, i.e. non-quoted text.
  ;;
  ;;     (make-string-list foo bar dii)
  ;;
  (define-syntax make-string-list
    (lambda (x)
      (let* ((stx (syntax->datum x))
             (-> datum->syntax))
        #`(map symbol->string (quote #,(-> x (cdr stx)))))))


  (define (sequence start len . rest)
    (let ((step (if (pair? rest) (car rest) 1)))
      (let loop ((num start))
        (if (< num (+ len start))
            (cons num (loop (+ num step)))
            '()))))

  (define (range start end . rest)
    (let ((step (if (pair? rest) (car rest) 1)))
      (let loop ((num start))
        (if (< num end)
            (cons num (loop (+ num step)))
            '()))))


  )

