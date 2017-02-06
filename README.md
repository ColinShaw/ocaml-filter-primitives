# Filter Primatives in OCaml

This is just a collection of OCaml fragments for FIR filters so that one can see what the compiler produces.  The point is to 
evaluate the expresiveness of the code versus the speed of the compiled code.  All the stops are pulled with type hinting so that
the OCaml compiler is able to make good decisions.  However, it retains expressiveness in the way it abstracts kernels.  
