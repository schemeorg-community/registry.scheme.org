;; Feature identifiers for (cond-expand) and (features)

;; R7RS Standard Feature Identifiers

(id r7rs)
(description "All R7RS Scheme implementations have this feature")

(id exact-closed)
(description "The algebraic operations +, -, *, and expt (where the second argument is a non-negative integer) produce exact values given exact inputs")

(id exact-complex)
(description "Exact complex numbers are provided")

(id ieee-float)
(description "Inexact numbers are IEEE 754 binary floating point values")

(id full-unicode)
(description "All Unicode characters present in Unicode version 6.0 are supported as Scheme characters")

(id ratios)
(description "/ with exact arguments produces an exact result when the divisor is nonzero")

(id jvm)
(description "Running on a Java Virtual Machine (JVM)")

(id clr)
(description "Running on a Common Language Runtime (.NET CLR)")

(id llvm)
(description "Running on the LLVM compiler infrastructure")

(id ilp32)
(description "C language int, long and pointer types are all 32 bits wide")

(id lp64)
(description "C language int type is 32 bits, long and pointer 64 bits wide")

(id ilp64)
(description "C language int, long, and pointer types are all 64 its wide")

(id big-endian)
(description "The CPU byte order is big-endian")

(id little-endian)
(description "The CPU byte order is little-endian")
