;; Feature identifiers for (cond-expand) and (features)

;; R7RS Standard Feature Identifiers

((id "r7rs")
 (description "All R7RS Scheme implementations have this feature"))

((id "exact-closed")
 (description "The algebraic operations +, -, *, and expt (where the second argument is a non-negative integer) produce exact values given exact inputs"))

((id "exact-complex")
 (description "Exact complex numbers are provided"))

((id "ieee-float")
 (description "Inexact numbers are IEEE 754 binary floating point values"))

((id "full-unicode")
 (description "All Unicode characters present in Unicode version 6.0 are supported as Scheme characters"))

((id "ratios")
 (description "/ with exact arguments produces an exact result when the divisor is nonzero"))

((id "jvm")
 (description "Running on a Java Virtual Machine (JVM)"))

((id "clr")
 (description "Running on a Common Language Runtime (.NET CLR)"))

((id "llvm")
 (description "Running on the LLVM compiler infrastructure"))

((id "ilp32")
 (description "C language int, long and pointer types are all 32 bits wide"))

((id "lp64")
 (description "C language int type is 32 bits, long and pointer 64 bits wide"))

((id "ilp64")
 (description "C language int, long, and pointer types are all 64 its wide"))

((id "big-endian")
 (description "The CPU byte order is big-endian"))

((id "little-endian")
 (description "The CPU byte order is little-endian"))

;; Others

((id "64bit")
 (description "Running on a 64-bit CPU (note: symbol starts with number)"))

((id "complex")
 (description "Complex numbers are provided"))

((id "clang")
 (description "LLVM clang compiler [Chicken]"))

((id "crypto")
 (description "Crypto interface [Sagittarius]"))

((id "csi")
 (description "Chicken Scheme interpreter"))

((id "dload")
 (description "Dynamic loading of C libraries [Chicken]"))

((id "dynamic-loading")
 (description "Dynamic loading of C libraries [Chibi]"))

((id "ffi")
 (description "[Sagittarius]"))

((id "ffi-vargs")
 (description "[Sagittarius]"))

((id "full-numeric-tower")
 (description "All RnRS number types are implemented [Chicken]"))

((id "manyargs")
 (description "[Chicken]"))

((id "mit/gnu")
 (description "MIT/GNU Scheme"))

((id "modules")
 (description "[Chibi]"))

((id "odbc")
 (description "ODBC interface [Sagittarius]"))

((id "ptables")
 (description "[Chicken]"))

((id "socket")
 (description "Sockets interface [Sagittarius]"))

((id "srfi-*")
 (description "Scheme Request for Implementation number N implemented"))

((id "swank")
 (description "Common Lisp SWANK remote REPL protocol supported"))

((id "termios")
 (description "Termios interface [Sagittarius]"))

((id "threads")
 (description "Multi-threading is supported (either green threads or OS threads)"))

((id "regexp-backrefs")
 (description "[SRFI 115]"))

((id "regexp-look-around")
 (description "[SRFI 115]"))

((id "regexp-non-greedy")
 (description "[SRFI 115]"))

((id "regexp-unicode")
 (description "[SRFI 115]"))

((id "syntax-case")
 (description "syntax-case macros are available"))

((id "uvector")
 (description "[Chibi]"))

((id "zlib")
 (description "Zlib interface [Sagittarius]"))
