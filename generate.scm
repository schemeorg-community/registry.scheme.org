(import (scheme base) (scheme file) (scheme read) (scheme write))

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

(define (assoc1 key alist)
  (let ((x (assoc key alist)))
    (if (and (list? x) (= 2 (length x))) (cadr x) (error "Nope"))))

(define (display-sxml x)
  (define (display* . xs) (for-each display xs))
  (define (display-attribute attribute)
    (display* " " (car attribute) "=" "\"" (cdr attribute) "\""))
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
         (display x))
        (else (error "Bad"))))

;;

(define (scheme-id)
  `((h2 "Scheme ID")
    (table
     (tr
      (th "ID")
      (th "Title")
      (th "Contact"))
     ,@(map (lambda (entry)
              `(tr (td (code ,(symbol->string (assoc1 'id entry))))
                   (td ,(assoc1 'title entry))
                   (td ,(assoc1 'contact entry))))
            (group 'id (with-input-from-file "scheme-id.scm" read-all))))))

(define (display-page)
  (display-sxml
   `(html
     (head
      (title "Scheme registry")
      (style "body { font-family: sans-serif; }"
        "table, th, td { border: 1px solid black; }"
        "table { border-collapse: collapse; }"))
     (body
      (h1 "Scheme registry")
      (p "The Scheme registry collects identifiers.")
      ,@(scheme-id)))))

(with-output-to-file "index.html" display-page)
