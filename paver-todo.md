# PolyPaver Roadmap
(Log is below)

## release 0.2

### core package release ASAP

* SPARK examples

    * adapt and add erfriemann

* tidy up options and their --help documentation

    * remove eps setting options

    * check and document option combinations

        * -f and -k

        * -I and -k

* minimal update of public documentation

* produce binaries for multiple platforms

    * OSX, Windows 32 bit, Linux 32 and 64 bit

### SPARK preprocessor

* speed-up Ada parser

    * deal with the ambiguity in expression_or_discrete_range

* pass 1: determine dependency order among the ads files

* pass 2: extract exported types and function return types from ads files

* pass 3: drill down to all expressions, chaning operators

    * tracking scopes of variables and parameters

    * tracking all locally visible types and functions

## release 0.5

* Use aern-0.5.0

## future releases

* inclusion and inequality transitive substituting formula preprocessor

* MetiTarski parser

* TPTP parser

* support quantifiers

# PolyPaver log

## release 0.1

* revision 1441 in FPByContract