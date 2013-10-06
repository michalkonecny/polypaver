# PolyPaver Roadmap
(Log is below)


* produce OS X binaries for release 0.2

## release 0.2.1

* UI

    * print help menu when given incorrect parameters

* SPARK preprocessor

    * pass 1: determine dependency order among the ads files

    * pass 2: recognise imported types and function signatures

    * pass 3: drill down to all expressions, changing operators
    
        * tracking scopes of variables and parameters

        * tracking all locally visible types and functions

* support FP types of any precision

    * merge ...Floats and ...Long_Floats into one package ...FP

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

## release 0.2

* revision 1831 in FPByContract

* DONE deal with universal_real* constants arising in SPARK VCs 

* DONE SPARK examples

    * DONE adapt and add erfriemann

    * DONE deal with universal_real* in the peak example

    * DONE wiki on how to work with SPARK, using the supplied examples

        * DONE how the VCs were derived

        * DONE how to prove the VCs using PolyPaver
        
        * DONE migrate to Google Code
        
        * DONE polish it up
        
* DONE page SyntaxPP
    
    * DONE grammar for pp input language
    
    * DONE polish, maybe add some text
        
* DONE produce binaries for multiple platforms

    * Windows 7 (DONE), Windows 8 (Jan+Adam)
    
    * Linux 12.04 32 bit (DONE) and 12.04 64 bit (Jan)

    * clearly state prerequisites of each binary - use a fresh install to determine and verify

* DONE rewrite page Installation
  * at the end add #sidebar TableOfContents

* DONE rewrite page Releases
  * at the end add #sidebar TableOfContents
        
* DONE update top level documentation on Google Code
    * probably hide some of the old stuff (perhaps temporarily)
            
## release 0.1

* revision 1441 in FPByContract