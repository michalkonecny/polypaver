/*
    An example problem where the subdivision has to be fine enough to
    separate a hypothesis from a conclusion whose false/true regions
    are very close to one another.  
    Thus the paving has to fit into the gap between the two, no
    matter how good the solving is on each box.  Consequently,
    increasing the polynomial degree usually does not help.
    Plain interval arithmetic performs the best without further options.
    
    The experimental extensions -g (guessing of helpful split direction)
    and -k (box skewing) can sometimes speed up the proof but
    sometimes do not compensate for the slow down caused by having
    to use higher order polynomials.  (See skewing2.pp for such an example.)
    
    When using -k, it is usually necessary to also use -m, which switches
    off solving of large boxes and starts solving only for boxes that
    are obtained with the given number of splits.
    The idea of -k is that boxes are skewed along difficult-to-solve 
    hyper-planes.  Such hyperplanes arise here due to the small gap
    between the hypothesis and conclusion.  On large boxes in which the
    hyperplanes are too curved or there are multiple such hyperplanes,
    the current algorithm seems to get confused.
    
    Proved eg with:
         -d 0            (9703 boxes, 9.5s)
         -d 1            (9703 boxes, 15.9s)
         -d 1 -g         (7279 boxes, 13.2s)
         -d 1 -k -m 8    (7371 parallelepipeds, 23.8s)
         -d 1 -g -k -m 8 (1599 parallelepipeds, 4.3s)
*/
oval.
H1: x <- [-1 .. 1].
H1: y <- [-1 .. 1].
H2: 1.1*x^2 + 1.5*y^2 = 1.
->
C2: 1.1*x^2 + 1.5*y^2 <- 1 + [-0.01 .. 0.01].
