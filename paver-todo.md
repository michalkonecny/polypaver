# PolyPaver Roadmap
(Log is below)

## release 0.2

* DONE deal with universal_real* constants arising in SPARK VCs 

* DONE SPARK examples

    * DONE adapt and add erfriemann

    * DONE deal with universal_real* in the peak example

    * DONE wiki on how to work with SPARK, using the supplied examples

        * DONE how the VCs were derived

        * DONE how to prove the VCs using PolyPaver
        
        * DONE migrate to Google Code
        
        * DONE polish it up
        
* page SyntaxPP
    
    * DONE grammar for pp input language
    
    * polish, maybe add some text
		
* produce binaries for multiple platforms

    * OSX (Jan), Windows 7 (DONE), Windows 8 (Jan+Adam)
    
    * DONE Linux 12.04 32 bit (DONE) and 12.04 64 bit (Jan)

    * clearly state prerequisites of each binary - use a fresh install to determine and verify

		* DONE Linux 12.04 64 bit 

* rewrite page Installation

  * at the end add #sidebar TableOfContents

* rewrite page Releases

  * at the end add #sidebar TableOfContents

* update top level documentation on Google Code

    * probably hide some of the old stuff (perhaps temporarily)
    
* announce to various people and on some forums


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

    * come up with a good top-level algorithm for trying various substitutions

        * identify possible substitutions - inequalities require monotonicity

    * make a totally silent version of the main loop

* MetiTarski parser

* TPTP parser

* support quantifiers

# PolyPaver log

## release 0.1

* revision 1441 in FPByContract