# VC files and examples

## Grammar

	<theorem>         ::= <identifier> "." [ { <hypothesis> } "->" ] <conclusion> { <conclusion> }
	<hypothesis>      ::= "H" { <id> } ":" <predicate> "."
	<conclusion>      ::= "C" { <id> } ":" <predicate> "."
	<predicate>       ::= <type_delcataion> | <expression> <relation_symbol> <expression> | ...
	<type_delcataion> ::= "integer(" <identifier> ")"
	<relation_symbol> ::= ...
	<expression>      ::= ...
	

### Example

	power.
	H1: x <- [1 .. 10].
	H2: n <- [1 .. 10].
	H3: integer(n).
	->
	C1: x^2 <= x^(n+1) + 0.000000001.

## Proving the easy conclusions

To categorise the easy problems, first run

    > polypaver power.vc -t 1 -q

The above command applies PolyPaver for 1s on default settings to each  conclusion in the file.

PolyPaver will output a summary in the end where we can find out
which conclusions have been proved and which not.

### Identifying easy conclusions

Those problems that were not decided due to reaching a maximum require increasing
some parameters other than time.
The success rate is improved by switching from the default degree 0 enclosures
to affine enclosures:

    > polypaver power.vc ...

For those problems where PolyPaver timed out, the output shows how far it got at proving it.
Typically, if the fraction is above 1 percent, it is possible to decide the problem in reasonably time 
using the same settings.  For example, running

    > polypaver ...

results in:

    >>>>>>>>>>> SUMMARY <<<<<<<<<<<
	...

## Proving the hard problems

...