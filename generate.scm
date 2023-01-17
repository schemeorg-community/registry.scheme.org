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

(define registry-old-ids
  '(("character-names" "character-name")
    ("filename-extensions" "filename-extension")
    ("library-names" "library-name")
    ("library-names-scheme" "library-name-scheme")
    ("machines" "machine")
    ("operating-systems" "operating-system")
    ("scheme-standards" "scheme-standard")
    ("version-properties" "version-flag-property")))

(define (registry registry-title registry-id intro table)
  `(section
    (@ (class "h-x-registry")
       (data-p-id ,registry-id))
    ,@(map (lambda (old-id) `(span (@ (id ,old-id))))
           (let ((entry (assoc registry-id registry-old-ids)))
             (if entry (cdr entry) '())))
    (h2 (@ (class "p-title") (id ,registry-id))
        ,registry-title)
    (p "Registry ID: "
       (a (@ (class "registry-anchor")
             (href ,(string-append "#" registry-id)))
          (code (@ (class "p-id"))
                ,registry-id)))
    (p "Source file: "
       ,(let ((filename (string-append registry-id ".pose")))
          `(a (@ (href ,filename))
              (code (@ (class "p-source"))
                    ,filename))))
    ,intro
    ,table))

;;

(define (scheme-standards-registry)
  (registry
   "Scheme standards"
   "scheme-standards"
   '(p)
   (tabulate
    '("ID" "Name" "Year" "Series")
    (map (lambda (entry)
           (let ((year (assoc? 'year entry))
                 (series (assoc? 'series entry)))
             (append (the-usual entry)
                     (list (list (if year (number->string year) ""))
                           (list (or series ""))))))
         (read-all-from-file "scheme-standards.pose")))))

(define (scheme-id-registry)
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

(define (operating-systems-registry)
  (registry
   "Operating systems"
   "operating-systems"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual
         (sort-by-id (read-all-from-file "operating-systems.pose"))))))

(define (machines-registry)
  (registry
   "Machines"
   "machines"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id (read-all-from-file "machines.pose"))))))

(define (splice-implementations)
  (classify "red" (read-all-from-file "scheme-id.pose")))

(define (splice-operating-systems)
  (classify "green" (read-all-from-file "operating-systems.pose")))

(define (splice-machines)
  (classify "blue" (read-all-from-file "machines.pose")))

(define (features-registry)
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

(define (cond-expand-registry)
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

(define (library-names-registry)
  (registry
   "Library name prefixes"
   "library-names"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual (sort-by-id
                    (append (read-all-from-file "library-names.pose")
                            (splice-implementations)))))))

(define (library-names-scheme-registry)
  (registry
   "Library names under (scheme ...)"
   "library-names-scheme"
   '(p)
   (tabulate
    '("ID" "Description")
    (map the-usual
         (sort-by-id (read-all-from-file "library-names-scheme.pose"))))))

(define (character-names-registry)
  (registry
   "#\\ character names"
   "character-names"
   '(p)
   (tabulate
    '("ID" "Escape" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code ,(assoc1 'id entry)))
                   ((code ,(or (assoc? 'string-escape entry) "")))
                   ,(format-description entry))))
         (sort-by-id (read-all-from-file "character-names.pose"))))))

(define (hash-syntax-registry)
  (registry
   "# lexical syntax"
   "hash-syntax"
   '(p "A quote ' next to a standard's or implementation's name"
       " means the syntax does not yield a self-evaluating object"
       " there, and must be quoted to form a valid expression.")
   (tabulate
    '("ID" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code ,(assoc1 'id entry)))
                   ,(format-description entry))))
         (read-all-from-file "hash-syntax.pose")))))

(define (hash-bang-syntax-registry)
  (registry
   "#! lexical syntax"
   "hash-bang-syntax"
   '(p)
   (tabulate
    '("ID" "Role" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code "#!" ,(assoc1 'id entry)))
                   (,(symbol->string (assoc1 'role entry)))
                   ,(format-description entry))))
         (sort-by-id (read-all-from-file "hash-bang-syntax.pose"))))))

(define (filename-extensions-registry)
  (registry
   "Filename extensions"
   "filename-extensions"
   '(p)
   (tabulate
    '("Extension" "Stands for" "Description")
    (map (lambda (entry)
           (cons (assoc? 'class entry)
                 `(((code ,(assoc1 'id entry)))
                   (,(assoc1 'stands-for entry))
                   ,(format-description entry))))
         (read-all-from-file "filename-extensions.pose")))))

(define (version-properties-registry)
  (registry
   "Version properties"
   "version-properties"
   '(p (a (@ (href "https://srfi.schemers.org/srfi-176/"))
          "SRFI 176"))
   (tabulate
    '("ID" "Description" "Type")
    (map (lambda (entry)
           (append (the-usual entry) (list (list (assoc1 'type entry)))))
         (sort-by-id (read-all-from-file "version-properties.pose"))))))

(define (log-message-fields-registry)
  (registry
   "Log message fields"
   "log-message-fields"
   '(p (a (@ (href "https://srfi.schemers.org/srfi-215/"))
          "SRFI 215"))
   (tabulate
    '("Symbol" "Mandatory" "Meaning" "Reference")
    (map (lambda (entry)
           (cons #f
                 `(((code ,(assoc1 'id entry)))
                   (,(symbol->string (assoc1 'mandatory entry)))
                   (,(assoc1 'meaning entry))
                   (,(assoc1 'reference entry)))))
         (read-all-from-file "log-message-fields.pose")))))

(define (codesets-registry)
  (registry
   "Codesets"
   "codesets"
   '(p (a (@ (href "https://srfi.schemers.org/srfi-238/"))
          "SRFI 238"))
   (tabulate
    '("ID" "Description" "Examples")
    (map (lambda (entry)
           (append (the-usual entry)
                   `(((code ,(assoc1 'examples entry))))))
         (sort-by-id (read-all-from-file "codesets.pose"))))))

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
      ,(scheme-standards-registry)
      ,(scheme-id-registry)
      ,(operating-systems-registry)
      ,(machines-registry)
      ,(features-registry)
      ,(cond-expand-registry)
      ,(library-names-registry)
      ,(library-names-scheme-registry)
      ,(character-names-registry)
      ,(hash-syntax-registry)
      ,(hash-bang-syntax-registry)
      ,(filename-extensions-registry)
      ,(version-properties-registry)
      ,(log-message-fields-registry)
      ,(codesets-registry)))))

(with-output-to-file "index.html" display-page)
