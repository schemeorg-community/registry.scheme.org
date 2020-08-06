(import (scheme base) (scheme char) (scheme file)
        (scheme read) (scheme write))
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

(define (sort-by-string-id entries)
  (list-sort (lambda (a b) (string<? (assoc1 'id a) (assoc1 'id b)))
             entries))

(define (classify class entries)
  (map (lambda (entry) `((class ,class) ,@entry))
       entries))

(define (tabulate column-headings rows)
  `(table (tr ,@(map (lambda (heading) `(th ,heading))
                     column-headings))
          ,@(map (lambda (row)
                   (let ((class (car row))
                         (tds (map (lambda (column heading)
                                     `(td (@ (class
                                               ,(string-append
                                                 "p-"
                                                 (string-downcase heading))))
                                          ,column))
                                   (cdr row)
                                   column-headings)))
                     `(tr (@ (class ,(if class
                                         (string-append "h-x-entry" " " class)
                                         "h-x-entry")))
                          ,@tds)))
                 rows)))

(define (the-usual entry)
  (cons (assoc? 'class entry)
        `((code ,(symbol->string (assoc1 'id entry)))
          ,(assoc1 'description entry))))

(define (registry registry-title registry-id intro table)
  `(section
    (@ (class "h-x-registry")
       (data-p-id ,registry-id))
    (h2 (@ (class "p-title"))
        ,registry-title)
    (p "Registry ID: " (code (@ (class "p-id")) ,registry-id))
    ,intro
    ,table))

;;

(define (scheme-id)
  (registry
   "Scheme implementations"
   "scheme-id"
   '(p "Scheme IDs for use in "
       (code "features") ", " (code "cond-expand") ", and many other places.")
   (tabulate
    '("ID" "Name" "Contact")
    (map (lambda (entry)
           (append (the-usual entry) (list (assoc1 'contact entry))))
         (sort-by-id (group-file 'id "scheme-id.scm"))))))

(define (operating-system)
  (registry
   "Operating systems"
   "operating-system"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id (group-file 'id "operating-system.scm"))))))

(define (machine)
  (registry
   "Machines"
   "machine"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id (group-file 'id "machine.scm"))))))

(define (splice-implementations)
  (classify "red" (group-file 'id "scheme-id.scm")))

(define (splice-operating-systems)
  (classify "green" (group-file 'id "operating-system.scm")))

(define (splice-machines)
  (classify "blue" (group-file 'id "machine.scm")))

(define (feature)
  (registry
   "Feature identifiers"
   "features"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id (append (group-file 'id "features.scm")
                                       (splice-implementations)
                                       (splice-operating-systems)
                                       (splice-machines)))))))

(define (library-name)
  (registry
   "Library name prefixes"
   "library-name"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id
                    (append (group-file 'id "library-name.scm")
                            (splice-implementations)))))))

(define (character-name)
  (registry
   "Character names"
   "character-name"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id (group-file 'id "character-name.scm"))))))

(define (hash-syntax)
  (registry
   "# lexical syntax"
   "hash-syntax"
   '(p)
   (tabulate
    '("ID" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `((code ,(assoc1 'id entry))
                   ,(assoc1 'description entry))))
         (sort-by-string-id (group-file 'id "hash-syntax.scm"))))))

(define (hash-bang-syntax)
  (registry
   "#! lexical syntax"
   "hash-bang-syntax"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id (group-file 'id "hash-bang-syntax.scm"))))))

(define (foreign-status-set)
  (registry
   "Foreign status sets"
   "foreign-status-set"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual
         (sort-by-id (group-file 'id "foreign-status-set.scm"))))))

(define (foreign-status-property)
  (registry
   "Foreign status properties"
   "foreign-status-property"
   '(p)
   (tabulate
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
      ,(scheme-id)
      ,(operating-system)
      ,(machine)
      ,(feature)
      ,(library-name)
      ,(character-name)
      ,(hash-syntax)
      ,(hash-bang-syntax)
      ,(foreign-status-set)
      ,(foreign-status-property)))))

(with-output-to-file "index.html" display-page)
