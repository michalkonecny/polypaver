{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-|
    Module      :  Numeric.ER.BasicTypes.DomainBox
    Description :  portions of many-dimensional domains   
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Abstractions of the 'Box' datatype, often used to represent
    sections of multi-dimensional function domains.
    
    To be imported qualified, usually with prefix DBox.
    
    VariableID(..) and DomainBox 
    are usually imported separately and not qualified.
-}
module Numeric.ER.BasicTypes.DomainBox
(
    VariableID(..),
    getNVars,
    DomainBox(..),
    DomainBoxMappable(..),
    DomainIntBox(..)
)
where

import Numeric.ER.BasicTypes

import qualified Data.Set as Set
import qualified Data.Map as Map

import Prelude hiding (lookup)


{-| 
    A class abstracting a type of variable identifiers 
    for axes in function domains, polynomials etc.
-}
class (Ord varid) => VariableID varid
    where
    newVarID :: Set.Set varid -> varid
    defaultVar :: varid
    defaultVar = newVarID Set.empty
    showVar :: varid -> String

getNVars :: (VariableID varid) => Int -> [varid]
getNVars n =
    aux (Set.empty) n
    where
    aux prevVars n 
        | n > 0 = 
            aux (Set.insert (newVarID prevVars) prevVars) (n - 1)
        | n == 0 =
            Set.toAscList $ prevVars 

{-|
    A class abstracting a type of many-dimensional points, intervals
    or anything indexed by a subset of dimensions.
    
    More generally, this class abstracts most of 'Data.Map.Map'.
-}
class (VariableID varid) => DomainBox box varid val
    | box -> varid val, varid val -> box
    where
    noinfo :: box
    isNoinfo :: box -> Bool
    size :: box -> Int
    {-| constructor using 'defaultVar' -}
    unary :: val -> box
    singleton :: varid -> val -> box
    toList :: box -> [(varid, val)]
    fromList :: [(varid, val)] -> box
    toAscList :: box -> [(varid, val)]
    fromAscList :: [(varid, val)] -> box
--    toMap :: box -> Map.Map varid val
--    fromMap :: Map.Map varid val -> box
    compare :: (val -> val -> Ordering) -> box -> box -> Ordering
    adjust :: (val -> val) -> varid -> box -> box
    insert :: varid -> val -> box -> box
    insertWith :: (val -> val -> val) -> varid -> val -> box -> box
    delete :: varid -> box -> box
    member :: varid -> box -> Bool
    notMember :: varid -> box -> Bool
    union :: box -> box -> box
    unionWith :: (val -> val -> val) -> box -> box -> box
    keys :: box -> [varid]
    elems :: box -> [val]
    filter :: (val -> Bool) -> box -> box
    fold :: (val -> a -> a) -> a -> box -> a
    foldWithKey :: (varid -> val -> a -> a) -> a -> box -> a
    {-| 
        for all variables that appear in both boxes,
        apply the function and add the result to the list 
     -}
    zipWith :: (val -> val -> a) -> box -> box -> [(varid, a)] 
    {-| 
        For all variables that appear in either of the two boxes,
        apply the function and add the result to the list.
        
        Supply the default value when the variable is missing from either box. 
     -}
    zipWithDefault :: val -> (val -> val -> a) -> box -> box -> [(varid, a)] 
    {-| 
        For all variables that appear in the first box,
        apply the function and add the result to the list.
        
        Supply the default value when the variable is missing from the second box. 
     -}
    zipWithDefaultSecond :: val -> (val -> val -> a) -> box -> box -> [(varid, a)] 
    findWithDefault :: val -> varid -> box -> val
    {-|
        Pick the extents of a single variable in a domain box.
        If there is no information for this variable, assume the
        variable ranges over the whole real line.
    -}
    lookup ::     
        String {-^ identification of caller location to use in error messages -} ->
        varid ->
        box ->
        val
        
{-|
    A class linking two domain box types that share the
    index type so that boxes of the two types can be
    converted etc.
-}
class (DomainBox box1 varid val1, DomainBox box2 varid val2) => 
    DomainBoxMappable box1 box2 varid val1 val2
    where
    map :: (val1 -> val2) -> box1 -> box2
    mapWithKey :: (varid -> val1 -> val2) -> box1 -> box2
    intersectionWith :: (val1 -> val2 -> val1) -> box1 -> box2 -> box1
    difference :: box1 -> box2 -> box1 

{-|
    A class abstracting a type of many-dimensional intervals.
-}
class (DomainBox box varid ira) => DomainIntBox box varid ira
    | box -> varid ira, varid ira -> box
    where
    {-|
        Check whether the two domains specify the same
        interval for each variable that they share.
    -}
    compatible ::
        box ->
        box ->
        Bool
    {-|
        Assuming that two domains are compatible, take the
        most information from both of the domains about the
        ranges of variables.
    -}
    unify ::
        String {-^ identification of caller location to use in error messages -} ->
        box ->
        box ->
        box
    {-|
        Find the variable with the largest interval
        and return it together with the default splitting point
        in its domain.
    -}
    bestSplit ::
        box  {-^ box considered for splitting -} ->
        (varid, (ira, ira))
        {-^ variable with widest domain, its domain and default split point -}
    split ::
        box {-^ box to split -} ->
        varid {-^ direction to split in -} ->
        Maybe ira  {-^ point to split the domain of variable @varid@ at, if absent use default -} ->
        (box, box)
    classifyPosition ::
        box {-^ domain @d1@ -} ->
        box {-^ domain @d2@ -} ->
        (Bool, Bool, Bool, Bool) 
            {-^ 
                Answers to these (mutually exclusive) questions:
                
                * is @d1@ outside and /not/ touching @d2@?
            
                * is @d1@ outside and touching @d2@?
            
                * is @d1@ intersecting and not inside @d2@?
            
                * is @d1@ inside @d2@?
            -}
            