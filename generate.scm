(import (scheme base) (scheme file) (scheme read) (scheme write))
(import (srfi 132))

(define (read-all)
  (let loop ((xs '()))
    (let ((x (read)))
      (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

(define (group head xs)
  (define (eject gs g) (if (null? g) gs (cons (reverse g) gs)))
  (let loop ((xs xs) (gs '()) (g '()))
    (cond ((null? xs)
           (reverse (eject gs g)))
          ((and (pair? (car xs)) (equal? head (caar xs)))
           (loop (cdr xs) (eject gs g) (list (car xs))))
          (else
           (loop (cdr xs) gs (cons (car xs) g))))))

(define (group-file head filename)
  (group head (with-input-from-file filename read-all)))

(define (assoc? key alist)
  (let ((x (assoc key alist)))
    (cond ((not x) #f)
          ((and (list? x) (= 2 (length x))) (cadr x))
          (else (error "Nope")))))

(define (assoc1 key alist)
  (let ((x (assoc key alist)))
    (if (and (list? x) (= 2 (length x))) (cadr x) (error "Nope"))))

(define (display-sxml x)
  (define (display* . xs) (for-each display xs))
  (define (display-char char)
    (let* ((cc (char->integer char))
           (ok? (case char ((#\& #\< #\> #\") #f) (else (<= #x20 cc #x7e)))))
      (if ok? (display char) (display* "&#" cc ";"))))
  (define (display-attribute attribute)
    (display* " " (car attribute) "=\"")
    (string-for-each display-char (cadr attribute))
    (display "\""))
  (cond ((pair? x)
         (display* "<" (car x))
         (let ((body (cond ((and (pair? (cdr x))
                                 (pair? (cadr x))
                                 (eq? '@ (car (cadr x))))
                            (for-each display-attribute (cdr (cadr x)))
                            (cddr x))
                           (else (cdr x)))))
           (display ">")
           (for-each display-sxml body)
           (display* "</" (car x) ">")))
        ((string? x)
         (string-for-each display-char x))
        (else (error "Bad:" x))))

;;

(define (sort-by-id entries)
  (list-sort (lambda (a b)
               (string<? (symbol->string (assoc1 'id a))
                         (symbol->string (assoc1 'id b))))
             entries))

(define (classify class entries)
  (map (lambda (entry) `((class ,class) ,@entry))
       entries))

(define (tabulate column-headings rows)
  `(table (tr ,@(map (lambda (heading) `(th ,heading))
                     column-headings))
          ,@(map (lambda (row)
                   (let ((class (car row))
                         (tds (map (lambda (column) `(td ,column))
                                   (cdr row))))
                     (if class `(tr (@ (class ,class)) ,@tds)
                         `(tr ,@tds))))
                 rows)))

(define (the-usual entry)
  (cons (assoc? 'class entry)
        `((code ,(symbol->string (assoc1 'id entry)))
          ,(assoc1 'description entry))))

;;

(define (scheme-id)
  `((h2 "Scheme implementations")
    (p "Scheme IDs for use in "
       (code "features") ", " (code "cond-expand") ", and many other places.")
    ,(tabulate
      '("ID" "Name" "Contact")
      (map (lambda (entry)
             (append (the-usual entry) (list (assoc1 'contact entry))))
           (sort-by-id (group-file 'id "scheme-id.scm"))))))

(define (operating-system)
  `((h2 "Operating systems")
    ,(tabulate
      '("ID" "Description")
      (map the-usual (sort-by-id (group-file 'id "operating-system.scm"))))))

(define (machine)
  `((h2 "Machines")
    ,(tabulate
      '("ID" "Description")
      (map the-usual (sort-by-id (group-file 'id "machine.scm"))))))

(define (splice-implementations)
  (classify "red" (group-file 'id "scheme-id.scm")))

(define (splice-operating-systems)
  (classify "green" (group-file 'id "operating-system.scm")))

(define (splice-machines)
  (classify "blue" (group-file 'id "machine.scm")))

(define (feature)
  `((h2 "Feature identifiers")
    ,(tabulate
      '("ID" "Description")
      (map the-usual (sort-by-id (append (group-file 'id "features.scm")
                                         (splice-implementations)
                                         (splice-operating-systems)
                                         (splice-machines)))))))

(define (library-name)
  `((h2 "Library name prefixes")
    ,(tabulate
      '("ID" "Description")
      (map the-usual (sort-by-id
                      (append (group-file 'id "library-name.scm")
                              (splice-implementations)))))))

(define (reader-directive)
  `((h2 "Reader directives")
    ,(tabulate
      '("ID" "Description" "Prefixes")
      (map (lambda (entry)
             (append (the-usual entry)
                     (list `(code ,(assoc1 'prefixes entry)))))
           (sort-by-id (group-file 'id "reader-directive.scm"))))))

(define (foreign-status-set)
  `((h2 "Foreign status sets")
    ,(tabulate
      '("ID" "Description")
      (map the-usual
           (sort-by-id (group-file 'id "foreign-status-set.scm"))))))

(define (foreign-status-property)
  `((h2 "Foreign status properties")
    ,(tabulate
      '("ID" "Description" "Type")
      (map (lambda (entry)
             (append (the-usual entry) (list (assoc1 'type entry))))
           (group-file 'id "foreign-status-property.scm")))))

(define (display-page)
  (display "<!doctype html>")
  (display-sxml
   `(html
     (@ (lang "en"))
     (head
      (title "Scheme Registry")
      (style ""
        "body { font-family: sans-serif; background-color: beige; }"
        "body { max-width: 40em; }"
        "table { border-collapse: collapse; }"
        "table, th, td { border: 1px solid black; }"
        "th, td { vertical-align: top; padding: 2px; }"
        "code { white-space: nowrap; }"
        "tr.red td { background-color: sandybrown; }"
        "tr.green td { background-color: lightgreen; }"
        "tr.blue td { background-color: lightblue; }"
        ))
     (body
      (h1 "Scheme Registry")
      (p "The Scheme registry collects identifiers.")
      ,@(scheme-id)
      ,@(operating-system)
      ,@(machine)
      ,@(feature)
      ,@(library-name)
      ,@(reader-directive)
      ,@(foreign-status-set)
      ,@(foreign-status-property)))))

(with-output-to-file "index.html" display-page)
