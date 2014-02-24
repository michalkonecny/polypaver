/* 
    A neat inequality found on the wikipedia page 
    http://en.wikipedia.org/wiki/Inequality_(mathematics).
    In the original inquality H1 and H2 are missing and
    H3 is simply a /= b.  Also, the original divides the
    left hand side by (b-a).

    PolyPaver cannot prove the original as its variables are not bounded 
    and there is touching at a = b.  Also, PolyPaver is currently much
    better at dealing with multiplication than division.  We have therefore
    multiplied both sides by (b-a) and ensured that b > a.

    This theorem can be proved for example with options: -d 2 -m 10
    To see also a plot while proving, add options -w 700 -h 700.

    Timing on 2014-02-24 using 32-bit Ubuntu 12.04, CPU i5 750 2.67GHz, 6GB RAM:
        Conjecture proved TRUE in 739.53s (0d, 0h, 12min, 19s).
        Proved fraction: [0.9999999999999999,1.0000000000005127]
        Computed boxes: 12425
        Greatest queue size: 20
        Greatest depth: 20
*/ 


wikip_eab.
H1: a <- [-10 .. 10].
H2: b <- [-10 .. 10].
H3: b > a + 0.1.
->
C1: (Exp(b) - Exp(a)) > (b-a) * Exp((a+b)/2).
