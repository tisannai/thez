#!chezscheme
(library (tuile guile)
  (export object->string
          string-concatenate
          string-join
          string-split
          delete
          unspecified?
          define-module
          scandir
          call/ec
          load-guile-lib
          primitive-load
          assoc-ref
          assoc-set!
          assoc-remove!
          identity
          negate
          ->bool
          floor/
          getcwd
          canonicalize-path
          )
  (import
    (chezscheme)
    (prefix (only (srfi :152) string-split string-join) srfi:)
    )

  (define (object->string x)
    (call-with-string-output-port
     (lambda (p) (put-datum p x))))

  (define (string-concatenate lst)
    (apply string-append lst))

  (define string-join srfi:string-join)

  (define (string-split str ch)
    (srfi:string-split str (string ch)))

  ;;  (define (string-join lst delim)
  ;;    (apply string-append (cons (car lst)
  ;;                               (map (lambda (i) (string-append delim i)) (cdr lst)))))

  (define delete remove)

  (define (unspecified? item)
    (eq? (if #f #t) item))

  ;; Eat the whole define-module with zero effects.
  (define-syntax define-module
    (lambda (x)
      #'#t))

  (define (scandir dir)
    (append (list "." "..") (directory-list dir)))

  (define call/ec call/cc)
  
  (define (datum->string datum)
    (with-output-to-string (lambda ()
                             (write datum))))

  (define primitive-load load)

  (define (load-guile-lib lib)
    (load (string-append (car (srfi:string-split (getenv "GUILE_LOAD_PATH") ":"))
                         "/"
                         lib)))

  (define (assoc-ref alist key)
    (let ((item (assoc key alist)))
      (if item
          (cdr item)
          #f)))

  (define (assoc-set! alist key value)
    (let loop ((rest alist)
               (head '()))
      (if (pair? rest)
          (if (equal? key (caar rest))
              (append (reverse head)
                      (list (cons key value))
                      (cdr rest))
              (loop (cdr rest)
                    (cons (car rest)
                          head)))
          (cons (cons key value)
                (reverse head)))))

  (define (assoc-remove! alist key)
    (let loop ((rest alist)
               (head '()))
      (if (pair? rest)
          (if (equal? key (caar rest))
              (append (reverse head)
                      (cdr rest))
              (loop (cdr rest)
                    (cons (car rest)
                          head)))
          (reverse head))))

  (define (identity obj) obj)

  (define (negate proc) (lambda args (not (apply proc args))))

  (define (->bool x) (not (not x)))

  (define (floor/ a b)
    (if (exact? a)
        (fxdiv-and-mod a b)
        (fldiv-and-mod a b)))

  (define getcwd current-directory)

  (define (canonicalize-path path)
    (let* ((parts (srfi:string-split path "/"))
           (cur (if (string=? (car parts) "")
                    (list)
                    (reverse (cdr (srfi:string-split (getcwd) "/"))))))
      (let loop ((cur cur)
                 (tail parts))
        (if (pair? tail)
            (cond
             ((string=? (car tail) "..")
              (loop (cdr cur)
                    (cdr tail)))
             ((string=? (car tail) ".")
              (loop cur
                    (cdr tail)))
             ((string=? (car tail) "")
              (loop cur
                    (cdr tail)))
             (else
              (loop (cons (car tail) cur)
                    (cdr tail))))
            (string-append "/" (srfi:string-join (reverse cur) "/"))))))

  )
