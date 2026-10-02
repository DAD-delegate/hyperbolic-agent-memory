Hyperbolic Agent Memory M0, Common Lisp (SBCL).

Python is not part of this tree.

Host: Steel Bank Common Lisp. The store and the compiler are Lisp.
The program is not Common Lisp. It is a 4-cell prefix ISA.

Every instruction is exactly (op a b c). No keywords, no rest args, no infix.

root  (root title 0 0)     identity node id 1 at the origin
add   (add parent kind text)   next id, farther from the origin than parent
save  (save path 0 0)
load  (load path 0 0)
check (check 0 0 0)        norms, distances, print M0-OK

Unused operand slots are 0. Kind is a bare symbol.
Parent pointers are stored on the node. Coordinates do not imply parentage.

Files: geom.lisp store.lisp compile.lisp run.lisp m0.sexp

Run from this directory:
sbcl --script run.lisp
