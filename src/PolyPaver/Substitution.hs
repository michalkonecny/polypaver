module PolyPaver.Substitution 
where

import PolyPaver.Form
import PolyPaver.Subterms

getAllStrentheningSubstitutions :: 
    Form TermHash -> [Form TermHash]
getAllStrentheningSubstitutions form =
    [] -- TODO
    {-
        1. Identify all hypotheses that are inclusions or inequalities.
        
        2. Extract possible substitutions in term of hashes and their monotonicity types. 
        
        3. Try to apply each substitution to the conclusion.
        
        4. Note substitutions that have an effect and, in particular, those that reduce the formula arity.
    -}