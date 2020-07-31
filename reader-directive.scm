(directive "#!bwp")
(purpose "broken-weak-pair object")
(implementations chezscheme)

(directive "#!eof")
(purpose "end-of-file object")
(implementations chezscheme chicken gambit)

(directive "#!fold-case")
(purpose "change to case insensitive identifiers")
(rnrs r7rs)

(directive "#!key")
(purpose "start keyword arguments in lambda list")
(srfi 89)

(directive "#!no-fold-case")
(purpose "change to case sensitive identifiers")
(rnrs r7rs)

(directive "#!optional")
(purpose "start optional arguments in lambda list")
(srfi 89)

(directive "#!r6rs")
(purpose "change to R6RS read syntax")
(rnrs r6rs)

(directive "#!r7rs")
(purpose "change to R7RS read syntax")
(implementations gauche)

(directive "#!rest")
(purpose "start rest argument in lambda list")
(srfi 89)
