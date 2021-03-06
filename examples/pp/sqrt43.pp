/* 
    A hand-written version of a VC derived by SPARK for the sqrt program.

    This theorem can be proved for example with options: -d 7 -z 20 -m 5
    To see also a plot while proving, add options -w 700 -h 700.

    Timing on 2014-02-24 using 32-bit Ubuntu 12.04, CPU i5 750 2.67GHz, 6GB RAM:
        Conjecture proved TRUE in 265.32s (0d, 0h, 4min, 25s).
        Proved fraction: [0.9999999999999997,1.000000000000035]
        Computed boxes: 9335
        Greatest queue size: 24
        Greatest depth: 27
*/ 
sqrt43.
// version 1 using inequalities:
// H1: 0.5 <= x /\ x <= 2.
// H2: 0 <= r /\ r <= 3.
// version 2 using intervals (equivalent to version 1):
H1: x <- [0.5 .. 2].
H2: r <- [0 .. 3].
H3: r <- [-x^2/4+x .. x^2/4+1].
H4: r = (0.5 (*) (r (+) x (/) r)).
->
C1: 0.5 (*) (r (+) x (/) r) |<-| (1+4*FepsiRel) * Sqrt(x).
// C1: (0.5 (*) (r (+) x (/) r)) |<-| (1+4*FepsiRel) * Sqrt(x).
// The above should be easier but PolyPaver finds it harder...
