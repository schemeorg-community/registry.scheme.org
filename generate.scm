;; SPDX-License-Identifier: CC0-1.0

;; For portability and ease of maintenance, please keep all Scheme
;; code in this self-contained file and stick to R7RS-small. Thanks!

(import (scheme base) (scheme char) (scheme file)
        (scheme read) (scheme write))

(define (list-sort < xs)
  (define (insert x xs)
    (if (null? xs) (list x)
        (if (< x (car xs)) (cons x xs)
            (cons (car xs) (insert x (cdr xs))))))
  (let loop ((xs xs))
    (if (null? xs) '() (insert (car xs) (loop (cdr xs))))))

(define (read-all)
  (let loop ((xs '()))
    (let ((x (read)))
      (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

(define (read-all-from-file filename)
  (with-input-from-file filename read-all))

(define (append-map f xs) (apply append (map f xs)))

(define (no-key key) (error "No key:" key))

(define (assoc? key alist)
  (let ((x (assoc key alist)))
    (cond ((not x) #f)
          ((and (list? x) (= 2 (length x))) (cadr x))
          (else (no-key key)))))

(define (assoc1 key alist)
  (let ((x (assoc key alist)))
    (if (and (list? x) (= 2 (length x))) (cadr x) (no-key key))))

(define (superscripts s)
  (let ((n (string-length s)))
    (let loop ((a 0) (b 0) (acc '()))
      (cond ((= n b a)
             (reverse acc))
            ((= n b)
             (loop b b (cons (substring s a b) acc)))
            ((char=? #\^ (string-ref s b))
             (loop (+ b 2) (+ b 2)
                   (append `((sup ,(string (string-ref s (+ b 1))))
                             ,(substring s a b))
                           acc)))
            (else (loop a (+ b 1) acc))))))

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
           (unless (memq (car x) '(meta))
             (display* "</" (car x) ">"))))
        ((string? x)
         (string-for-each display-char x))
        (else (error "Bad:" x))))

;;

(define (sort-by-id entries)
  (list-sort (lambda (a b) (string<? (assoc1 'id a) (assoc1 'id b)))
             entries))

(define (sort-by-string-id entries)
  (list-sort (lambda (a b) (string<? (assoc1 'id a) (assoc1 'id b)))
             entries))

(define (format-description entry)
  (superscripts (assoc1 'description entry)))

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
                                          ,@column))
                                   (cdr row)
                                   column-headings)))
                     `(tr (@ (class ,(if class
                                         (string-append "h-x-entry" " " class)
                                         "h-x-entry")))
                          ,@tds)))
                 rows)))

(define (the-usual entry)
  (cons (assoc? 'class entry)
        `(((code ,(assoc1 'id entry)))
          ,(format-description entry))))

(define (registry registry-title registry-id intro table)
  `(section
    (@ (class "h-x-registry")
       (data-p-id ,registry-id))
    (h2 (@ (class "p-title") (id ,registry-id))
        ,registry-title)
    (p "Registry ID: " (a (@ (class "registry-anchor")
                             (href ,(string-append "#" registry-id)))
                          (code (@ (class "p-id"))
                                ,registry-id)))
    ,intro
    ,table))

;;

(define (scheme-standard)
  (registry
   "Scheme standards"
   "scheme-standard"
   '(p)
   (tabulate
    '("ID" "Name" "Year")
    (map (lambda (entry)
           (let ((year (assoc? 'year entry)))
             (append (the-usual entry)
                     (list (list (if year (number->string year) ""))))))
         (read-all-from-file "scheme-standard.pose")))))

(define (scheme-id)
  (registry
   "Scheme implementations"
   "scheme-id"
   '(p "Scheme IDs for use in "
       (code "features") ", " (code "cond-expand") ", and many other places.")
   (tabulate
    '("ID" "Name" "Contact")
    (map (lambda (entry)
           (append (the-usual entry) (list (list (assoc1 'contact entry)))))
         (sort-by-id (read-all-from-file "scheme-id.pose"))))))

(define (operating-system)
  (registry
   "Operating systems"
   "operating-system"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual
         (sort-by-id (read-all-from-file "operating-system.pose"))))))

(define (machine)
  (registry
   "Machines"
   "machine"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id (read-all-from-file "machine.pose"))))))

(define (splice-implementations)
  (classify "red" (read-all-from-file "scheme-id.pose")))

(define (splice-operating-systems)
  (classify "green" (read-all-from-file "operating-system.pose")))

(define (splice-machines)
  (classify "blue" (read-all-from-file "machine.pose")))

(define (feature)
  (registry
   "Feature identifiers"
   "features"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id (append (read-all-from-file "features.pose")
                                       (splice-implementations)
                                       (splice-operating-systems)
                                       (splice-machines)))))))

(define (cond-expand-test)
  (registry
   "Tests cond-expand can do"
   "cond-expand"
   '(p)
   (tabulate
    '("ID" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code
                     "("
                     ,(assoc1 'id entry)
                     ,@(append-map (lambda (arg) `(" " ,arg))
                                   (cdr (assoc 'args entry)))
                     ")"))
                   ,(format-description entry))))
         (sort-by-id (read-all-from-file "cond-expand.pose"))))))

(define (library-name)
  (registry
   "Library name prefixes"
   "library-name"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id
                    (append (read-all-from-file "library-name.pose")
                            (splice-implementations)))))))

(define (library-name-scheme)
  (registry
   "Library names under (scheme ...)"
   "library-name-scheme"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual
         (sort-by-id (read-all-from-file "library-name-scheme.pose"))))))

(define (character-name)
  (registry
   "#\\ character names"
   "character-name"
   '(p)
   (tabulate
    '("ID" "Escape" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code ,(assoc1 'id entry)))
                   ((code ,(or (assoc? 'string-escape entry) "")))
                   ,(format-description entry))))
         (sort-by-id (read-all-from-file "character-name.pose"))))))

(define (hash-syntax)
  (registry
   "# lexical syntax"
   "hash-syntax"
   '(p "A quote ' next to an implementation's name"
       " means the syntax must be quoted in that implementation.")
   (tabulate
    '("ID" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code ,(assoc1 'id entry)))
                   ,(format-description entry))))
         (read-all-from-file "hash-syntax.pose")))))

(define (hash-bang-syntax)
  (registry
   "#! lexical syntax"
   "hash-bang-syntax"
   '(p)
   (tabulate
    '("ID" "Role" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code ,(assoc1 'id entry)))
                   (,(symbol->string (assoc1 'role entry)))
                   ,(format-description entry))))
         (sort-by-id (read-all-from-file "hash-bang-syntax.pose"))))))

(define (filename-extension)
  (registry
   "Filename extensions"
   "filename-extension"
   '(p)
   (tabulate
    '("Extension" "Stands for" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code ,(assoc1 'id entry)))
                   (,(assoc1 'stands-for entry))
                   ,(format-description entry))))
         (read-all-from-file "filename-extension.pose")))))

(define (version-flag-property)
  (registry
   "Version flag properties"
   "version-flag-property"
   '(p "SRFI 176")
   (tabulate
    '("ID" "Description" "Type")
    (map (lambda (entry)
           (append (the-usual entry) (list (list (assoc1 'type entry)))))
         (sort-by-id (read-all-from-file "version-flag-property.pose"))))))

(define (display-page)
  (display (string-append
            "<!doctype html>"
            "<!-- SPDX-License-Identifier: CC0-1.0 -->"
            "<!-- This page uses https://microformats.org/wiki/microformats2"
            " tagging and can be parsed by machine. -->"))
  (display-sxml
   `(html
     (@ (lang "en"))
     (head
      (title "Scheme Registry")
      (meta (@ (charset "UTF-8")))
      (style ""
        "body { font-family: sans-serif; background-color: beige;"
        " max-width: 40em; margin: 12px; }"
        "table { border-collapse: collapse; }"
        "table, th, td { border: 1px solid black; }"
        "th, td { vertical-align: top; padding: 2px; }"
        "code { white-space: nowrap; }"
        "tr.red td { background-color: sandybrown; }"
        "tr.green td { background-color: lightgreen; }"
        "tr.blue td { background-color: lightblue; }"
        ".intro { background-color: lightyellow; padding: 12px;"
        " border: 1px solid gray; border-radius: 10px; }"
        "a.registry-anchor, a.registry-anchor:visited { color: navy;"
        " border-bottom: 1px dashed navy; text-decoration: none; }"))
     (body
      (h1 "Scheme Registry")
      (p (@ (class "intro")) "A catalog of the defined symbols,"
         " identifiers and other pieces of data used by Scheme"
         " standards and implementations.")
      (p "Administration happens on the " (code "schemeregistry")
         " mailing list ("
         (a (@ (href "https://srfi-email.schemers.org/schemeregistry/"))
            "archives")
         ", "
         (a (@ (href ,(string-append
                       "https://srfi.schemers.org/srfi-list-subscribe.html"
                       "#schemeregistry")))
            "subscribe")
         "). Feel free to suggest additions and corrections there."
         " Source is in a "
         (a (@ (href "https://github.com/schemeorg/registry.scheme.org"))
            "git repository") ".")
      ,(scheme-standard)
      ,(scheme-id)
      ,(operating-system)
      ,(machine)
      ,(feature)
      ,(cond-expand-test)
      ,(library-name)
      ,(library-name-scheme)
      ,(character-name)
      ,(hash-syntax)
      ,(hash-bang-syntax)
      ,(filename-extension)
      ,(version-flag-property)))))

(with-output-to-file "index.html" display-page)
