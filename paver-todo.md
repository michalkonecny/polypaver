# PolyPaver Roadmap
(Log is below)

## release 0.2

* deal with universal_real* constants arising in SPARK VCs 

    * find out how they arise

    * give a helpful error message when coming across one

        * hint on what in the program is likely to have caused their apprearance 

        * why it is difficult for PolyPaver to work out what their value is

    * (postpone?) find out what their values should be

    * (postpone?) find a way to specify these values

* SPARK examples

    * DONE adapt and add erfriemann

    * remove any occurrences of universal_real* from the peak example

* DONE tidy up options and their --help documentation

* add FP sin and cos to the internal language and the SPARK PolyPaver package

    * look up suitable error bounds in the Ada specification

* minimal update of public documentation

* produce binaries for multiple platforms

    * OSX (Jan), Windows 32 bit (DONE), Linux 32 (DONE) and 64 bit (MK)

## release 0.2.1

* SPARK preprocessor

    * speed-up Ada parser

        * deal with the ambiguity in expression_or_discrete_range

    * pass 1: determine dependency order among the ads files

    * pass 2: extract exported types and function return types from ads files

    * pass 3: drill down to all expressions, chaning operators
    
        * tracking scopes of variables and parameters

        * tracking all locally visible types and functions

* support FP types of any precision

    * merge ...Floats and ...Long_Floats into one package ...Custom_Floats

        * each FP function to have two additional integer parameters mantissa_bits and max_exponent

        * the preprocessor will insert the correct values for these parameters

        * the preprocessor will read gnat.cfg to get these values right, otherwise assume IEEE

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