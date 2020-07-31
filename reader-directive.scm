(directive "#!bwp")
(purpose "broken-weak-pair object")
(implementations chezscheme)

(directive "#!eof")
(purpose "end-of-file object")
(implementations chezscheme chicken gambit)

(directive "#!fold-case")
(purpose "change to case insensitive identifiers")
(rnrs r7rs)

(directive "#!no-fold-case")
(purpose "change to case sensitive identifiers")
(rnrs r7rs)

(directive "#!r6rs")
(purpose "change to R6RS read syntax")
(rnrs r6rs)

(directive "#!r7rs")
(purpose "change to R7RS read syntax")
(implementations gauche)
