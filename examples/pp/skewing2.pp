/*
    An example problem where the subdivision has to be fine enough to
    separate a hypothesis from a conclusion whose false/true regions
    are very close to one another.  
    Thus the paving has to fit into the gap between the two no
    matter how good the solving is on each box.  Consequently,
    increasing the polynomial degree usually does not help.
    Plain interval arithmetic performs the best.

    The tool pp_simplify helps in cases such as these by performing
    a safe substitution, strengthening the formula while reducing its
    arity and eliminating the need to subdivide within a tight gap.
    
    Proved eg with:
         -d 0    (589 boxes, 0.5s)
         -d 4    (709 boxes, 52s)

    After processing with pp_simplify:
         -d 0    (137 boxes, 0.04s)
*/
skewing.
H1: x <- [0 .. 1].
H2: y <- [0 .. 1].
H3: y <= Sqrt(x).
->
C1: y <= Sqrt(x+0.01) + 0.01.
//pp_simplify converts this to: Sqrt(x) <= Sqrt(x+0.01) + 0.01
