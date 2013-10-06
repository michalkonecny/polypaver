{-|
    Module      :  Numeric.ER.RnToRm.BisectionTree.Integration
    Description :  abstract zipping of domain partitions used for integration
    Copyright   :  (c) 2007-2008 Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    To be imported qualified, usually with prefix BTINTEG.
-}
module Numeric.ER.RnToRm.BisectionTree.Integration 
(
    zipFromOrigin, zipOnSubdomain
)
where

import qualified Numeric.ER.RnToRm.BisectionTree as BISTR
import qualified Numeric.ER.Real.Approx as RA

import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox)
import Numeric.ER.BasicTypes
import Numeric.ER.Misc

--import qualified Data.Sequence as Seq
--import qualified Data.Map as Map
import Data.Maybe

{-|
    Transform a bunch of bisection trees over the same domain 
    by "integrating" them in a very abstract sense.  
    The trees are unified in their splitting patterns in the process.
    By supplying certain parameters, this function can in fact
    perform numerical integration of piece-wise polynomial functions.
    
    It can be also viewed as a "zipping+folding" operator over bisection trees that
    generates another bunch of bisection trees, synchronously traversing the original trees
    from a certain point on a selected orthogonal hyperplane outwards in both directions, 
    carrying some data along.
-}
zipFromOrigin ::
    (RA.ERIntApprox d, DomainIntBox box varid d, Show v1, Show v2, Show valPass) =>
    BISTR.ValueSplitter box varid d v1 ->
    BISTR.ValueCombiner box varid d v1 ->
    BISTR.ValueSplitter box varid d valPass ->
    BISTR.ValueCombiner box varid d valPass ->
    EffortIndex ->
    varid
        {-^ variable @x@ (ie axis or direction) to integrate in -} ->
    d 
        {-^ origin in terms of variable @x@ -} ->
    (Maybe d)
        {-^ support, ie the domain of @x@ on which to zip
            (automatically extended to include origin) -} ->
    (Maybe (BISTR.BisectionTree box varid d valPass) -> 
     Maybe (BISTR.BisectionTree box varid d valPass) -> 
     [BISTR.BisectionTree box varid d v1] -> 
     [BISTR.BisectionTree box varid d v2]) 
        {-^ what to do outside the support, 
            possibly being passed values from left/right
            when leaving the support -} ->
    (EffortIndex -> BISTR.Depth -> box -> [v1] -> [v2] -> Bool) 
        {-^ should a leaf be split? -} ->
    (EffortIndex -> BISTR.Depth -> box -> [v1] -> (valPass,[v2],valPass)) 
        {-^ integrator for a leaf containing the origin -} ->
    (EffortIndex -> BISTR.Depth -> box -> 
     (BISTR.BisectionTree box varid d valPass) -> [v1] -> 
     ([v2], BISTR.BisectionTree box varid d valPass))
        {-^ integrator over a leaf that sees the origin towards -infinity -} ->
    (EffortIndex -> BISTR.Depth -> box -> 
     [v1] -> (BISTR.BisectionTree box varid d valPass) -> 
     (BISTR.BisectionTree box varid d valPass, [v2])) 
        {-^ integrator over a leaf that sees the origin towards +infinity -} ->
    [BISTR.BisectionTree box varid d v1] 
        {-^ input functions -} ->
    [BISTR.BisectionTree box varid d v2]
        {-^ output functions
        
           The number of output functions does not have to be 
           the same as the number of input functions. 
        -}
zipFromOrigin
        valSplitter valCombiner valPassSplitter valPassCombiner 
        ix ivar origin
        maybeResultSupport outerValTransformer
        decideShouldSplit integrLeafOH integrLeafOL integrLeafOR
        bistrs =
    resultBistrs
    where
    (_, resultBistrs, _) = 
        integrateBistrOriginHere $ BISTR.syncMany valSplitter ix bistrs
    maybeSupport = -- extend resultSupport to cover the origin
        fmap extendToOrigin maybeResultSupport
        where
        extendToOrigin dom = origin RA.\/ dom
--        extendToOrigin domB =
--            case DBox.member ivar domB of
--                True -> DBox.insertWith (RA.\/) ivar origin domB
--                False -> domB
    -- the following function is used when we know the origin is within the current sub-domain:
    integrateBistrOriginHere bistrs@((BISTR.Leaf depth domB _) : _)
        | decideShouldSplit ix depth domB vals integrVals =  -- must descend
            integrateBistrOriginHere $ 
                map (BISTR.split valSplitter ix var pt domB) bistrs
        | otherwise =
            (Just lBistr, map (\v -> BISTR.Leaf depth domB v) integrVals, Just rBistr)
        where
        vals = map BISTR.bistrVal bistrs
        (var, (_,pt)) = DBox.bestSplit domB
        (lVal, integrVals, rVal) = integrLeafOH ix depth domB vals
        lBistr = BISTR.Leaf depth domBNoIVar lVal
        rBistr = BISTR.Leaf depth domBNoIVar rVal
        domBNoIVar = DBox.delete ivar domB
    integrateBistrOriginHere bistrs@((BISTR.Node depth domB var pt boundsLO boundsHI):_)
        | var /= ivar =
            integrateBistrOriginHereOrthogonal
        | origin `RA.refines` domLO =
            integrateBistrOriginHereLO
        | origin `RA.refines` domHI =
            integrateBistrOriginHereHI
        | otherwise = -- origin overlaps both sides
            -- have to amalgamate these trees:
            integrateBistrOriginHere $
                map (\b -> BISTR.Leaf depth domB (valCombiner ix depth b)) bistrs
        where
        domLO = DBox.lookup "BTINTEG: zipFromOrigin: Here: L: " var (BISTR.bistrDom boundsLO)
        domHI = DBox.lookup "BTINTEG: zipFromOrigin: Here: R: " var (BISTR.bistrDom boundsHI)
        integrateBistrOriginHereOrthogonal =
            -- apply integrateBistrOriginHere on both halves independently and glue them back together:
            (lBistrOr, bistrsIntgOr, rBistrOr)
            where
            bistrsIntgOr = 
                zipWith 
                    (\lo hi -> BISTR.Node depth domB var pt lo hi) 
                    boundsLOIntgOr boundsHIIntgOr
            lBistrOr =
                case (lBistrLOIntegrOr, lBistrHIIntegrOr) of
                    (Just lo, Just hi) -> 
                        Just $ BISTR.Node depth domBNoIVar var pt lo hi 
                    _ -> Nothing
            rBistrOr =
                case (rBistrLOIntegrOr, rBistrHIIntegrOr) of
                    (Just lo, Just hi) -> 
                        Just $ BISTR.Node depth domBNoIVar var pt lo hi 
                    _ -> Nothing
            domBNoIVar = DBox.delete ivar domB
            (lBistrLOIntegrOr, boundsLOIntgOr, rBistrLOIntegrOr) =
                integrateBistrOriginHere $ 
                    BISTR.syncMany valSplitter ix $ map BISTR.bistrLO bistrs
            (lBistrHIIntegrOr, boundsHIIntgOr, rBistrHIIntegrOr) =
                integrateBistrOriginHere $ 
                    BISTR.syncMany valSplitter ix $ map BISTR.bistrHI bistrs
            
        integrateBistrOriginHereHI =
--            unsafePrint 
--                ("BTINTEG: integrateBistrOriginHereHI: rDom = " ++ show rDom ++ 
--                 " origin = " ++ show origin ++
--                 " lValHI = " ++ show lValHI ++
--                 " rValHI = " ++ show rValHI) 
            -- recursion when origin is entirely to the right of the centre:
            (lBistrHI, bistrsIntgHI, rBistrHI)
            where
            bistrsIntgHI = 
                zipWith 
                    (\lo hi -> BISTR.Node depth domB var pt lo hi) 
                    lBoundsIntgHI rBoundsIntgHI 
            (lBistrHIHI, rBoundsIntgHI, rBistrHI) =
                integrateBistrOriginHere $ 
                    BISTR.syncMany valSplitter ix $ map BISTR.bistrHI bistrs
            (lBistrHI, lBoundsIntgHI) =
                integrateBistrOriginRight 
                    (BISTR.syncMany valSplitter ix $ map BISTR.bistrLO bistrs) 
                    lBistrHIHI
        integrateBistrOriginHereLO =
--            unsafePrint 
--                ("BTINTEG: integrateBistrOriginHereLO: lDom = " ++ show lDom ++ 
--                 " origin = " ++ show origin ++
--                 " lValLO = " ++ show lValLO ++
--                 " rValLO = " ++ show rValLO)
            -- recursion when origin is entirely to the left of the centre:
            (lBistrLO, bistrsIntgLO, rBistrLO)
            where
            bistrsIntgLO = 
                zipWith 
                    (\lo hi -> BISTR.Node depth domB var pt lo hi) 
                    lBoundsIntgLO rBoundsIntgLO 
            (lBistrLO, lBoundsIntgLO, rBistrLOLO) =
                integrateBistrOriginHere $ 
                    BISTR.syncMany valSplitter ix $ map BISTR.bistrLO bistrs
            (rBoundsIntgLO, rBistrLO) =
                integrateBistrOriginLeft 
                    rBistrLOLO
                    (BISTR.syncMany valSplitter ix $ map BISTR.bistrHI bistrs)
    -- the following function is used when we know 
    -- the origin is to the LEFT of the current sub-domain:
    integrateBistrOriginLeft Nothing bistrs = 
        -- previously detected as being outside the support
        (outerValTransformer Nothing Nothing bistrs, Nothing)
    integrateBistrOriginLeft (Just lBistr) bistrs@(bistr:_)
        | (isJust maybeSupport) &&
            RA.isInteriorDisjoint 
                (fromJust maybeSupport) 
                (DBox.findWithDefault RA.bottomApprox ivar (BISTR.bistrDom bistr)) =
--            (and $ Prelude.map snd $ 
--                DBox.zipWithDefaultSecond RA.bottomApprox RA.isInteriorDisjoint 
--                    (BISTR.bistrDom bistr) 
--                    (fromJust maybeSupport)) = 
            -- outside the integration domain 
            (outerValTransformer (Just lBistr) Nothing bistrs, Nothing)
    integrateBistrOriginLeft (Just lBistr) bistrs@((BISTR.Leaf depth domB _) : _)
        | decideShouldSplit ix depth domB vals integrVals = -- improve granularity by splitting
            integrateBistrOriginLeft (Just lBistr) $ 
                map (BISTR.split valSplitter ix var pt domB) bistrs
        | otherwise = 
            (map (\v -> BISTR.Leaf depth domB v) integrVals, 
             Just rBistr)
        where
        (var, (_,pt)) = DBox.bestSplit domB
        vals = map BISTR.bistrVal bistrs
        (integrVals, rBistr) =
            integrLeafOL ix depth domB lBistr vals
    integrateBistrOriginLeft mlBistr bistrs@((BISTR.Node depth domB var pt _ _):_) 
        | var /= ivar =
            integrateBistrOriginLeftOrthogonal
        | otherwise = -- var == ivar
            integrateBistrOriginLeftParallel
        where
        integrateBistrOriginLeftOrthogonal =
            -- apply integrateBistrOriginLeft on both halves independently and glue them back together:
            (bistrsIntg, mrBistr)
            where
            bistrsIntg = 
                zipWith 
                    (\lo hi -> BISTR.Node depth domB var pt lo hi) 
                    boundsLOIntg boundsHIIntg
            mrBistr =
                case (mrBistrLO, mrBistrHI) of
                    (Just lo, Just hi) -> 
                        Just $ BISTR.Node depth domBNoIVar var pt lo hi 
                    _ -> Nothing
            domBNoIVar = DBox.delete ivar domB
            (boundsLOIntg, mrBistrLO) =
                integrateBistrOriginLeft mlBistrLO $ 
                    BISTR.syncMany valSplitter ix $ map BISTR.bistrLO bistrs
            (boundsHIIntg, mrBistrHI) =
                integrateBistrOriginLeft mlBistrHI $ 
                    BISTR.syncMany valSplitter ix $ map BISTR.bistrHI bistrs
            (mlBistrLO, mlBistrHI) =
                case mlBistr of
                    Nothing -> (Nothing, Nothing)
                    Just lBistr -> 
                        (Just lo, Just hi)
                        where
                        BISTR.Node _ _ _ _ lo hi =
                            BISTR.split valPassSplitter ix var pt domB lBistr
            
        integrateBistrOriginLeftParallel =
            (bistrsIntg, mrBistr2)
            where
            bistrsIntg = 
                zipWith (\lo hi -> BISTR.Node depth domB var pt lo hi) lBoundsINT rBoundsINT 
            (lBoundsINT, mrBistr1) = 
                integrateBistrOriginLeft mlBistr $ 
                    BISTR.syncMany valSplitter ix $ map BISTR.bistrLO bistrs 
            (rBoundsINT, mrBistr2) =
                integrateBistrOriginLeft mrBistr1 $ 
                    BISTR.syncMany valSplitter ix $ map BISTR.bistrHI bistrs 

--    -- the following function is used when we know 
--    -- the origin is to the RIGHT of the current sub-domain:
    integrateBistrOriginRight bistrs Nothing = 
        -- previously detected as being outside the support
        (Nothing, outerValTransformer Nothing Nothing bistrs)
    integrateBistrOriginRight bistrs@(bistr:_) (Just rBistr)
        | (isJust maybeSupport) &&
            RA.isInteriorDisjoint 
                (fromJust maybeSupport) 
                (DBox.findWithDefault RA.bottomApprox ivar (BISTR.bistrDom bistr)) =
--            (and $ Prelude.map snd $ 
--                DBox.zipWithDefaultSecond RA.bottomApprox RA.isInteriorDisjoint 
--                    (BISTR.bistrDom bistr) 
--                    (fromJust maybeSupport)) = 
            -- outside the integration domain 
            (Nothing, outerValTransformer Nothing (Just rBistr) bistrs)
    integrateBistrOriginRight bistrs@((BISTR.Leaf depth domB _) : _) (Just rBistr)
        | decideShouldSplit ix depth domB vals integrVals = -- improve granularity by splitting
            integrateBistrOriginRight 
                (map (BISTR.split valSplitter ix var pt domB) bistrs)
                (Just rBistr)
        | otherwise = 
            (Just lBistr,
             map (\v -> BISTR.Leaf depth domB v) integrVals)
        where
        (var, (_,pt)) = DBox.bestSplit domB
        vals = map BISTR.bistrVal bistrs
        (lBistr, integrVals) =
            integrLeafOR ix depth domB vals rBistr
    integrateBistrOriginRight bistrs@((BISTR.Node depth domB var pt _ _):_) mrBistr
        | var /= ivar =
            integrateBistrOriginRightOrthogonal
        | otherwise = -- var == ivar
            integrateBistrOriginRightParallel
        where
        integrateBistrOriginRightOrthogonal =
            -- apply integrateBistrOriginLeft on both halves independently and glue them back together:
            (mlBistr, bistrsIntg)
            where
            bistrsIntg = 
                zipWith 
                    (\lo hi -> BISTR.Node depth domB var pt lo hi) 
                    boundsLOIntg boundsHIIntg
            mlBistr =
                case (mlBistrLO, mlBistrHI) of
                    (Just lo, Just hi) -> 
                        Just $ BISTR.Node depth domBNoIVar var pt lo hi 
                    _ -> Nothing
            domBNoIVar = DBox.delete ivar domB
            (mlBistrLO, boundsLOIntg) =
                integrateBistrOriginRight
                    (BISTR.syncMany valSplitter ix $ map BISTR.bistrLO bistrs)
                    mrBistrLO
            (mlBistrHI, boundsHIIntg) =
                integrateBistrOriginRight 
                    (BISTR.syncMany valSplitter ix $ map BISTR.bistrHI bistrs)
                    mrBistrHI
            (mrBistrLO, mrBistrHI) =
                case mrBistr of
                    Nothing -> (Nothing, Nothing)
                    Just rBistr -> 
                        (Just lo, Just hi)
                        where
                        BISTR.Node _ _ _ _ lo hi =
                            BISTR.split valPassSplitter ix var pt domB rBistr
            
        integrateBistrOriginRightParallel =
            (mlBistr2, bistrsIntg)
            where
            bistrsIntg = 
                zipWith (\lo hi -> BISTR.Node depth domB var pt lo hi) lBoundsINT rBoundsINT 
            (mlBistr2, lBoundsINT) = 
                integrateBistrOriginRight 
                    (BISTR.syncMany valSplitter ix $ map BISTR.bistrLO bistrs) mlBistr1 
            (mlBistr1, rBoundsINT) =
                integrateBistrOriginRight  
                    (BISTR.syncMany valSplitter ix $ map BISTR.bistrHI bistrs) mrBistr 

{-|
    Zip a list of bisection trees in synchrony but do something
    else inside and not inside a given subdomain.
    
    Further splitting at default points will be done up to the given depth
    in an attempt to separate the subdomain as well as possible.
    
    If the subdomain is not properly isolated by the splitting at the
    maximum depth, splits are made at irregular points to ensure full isolation
    of the subdomain.
-}
zipOnSubdomain ::
    (RA.ERIntApprox d, DomainIntBox box varid d) =>
    BISTR.ValueSplitter box varid d v1 ->
    EffortIndex ->
    BISTR.Depth 
        {-^ depth limit -} ->
    box
        {-^ subdomain @sd@ -} ->
    (box -> [v1] -> [v2])
        {-^ what to do with values /inside/ @sd@ -} ->
    (box -> [v1] -> [v2])
        {-^ what to do with values /outside/ @sd@ but /touching/ it -} ->
    (box -> [v1] -> [v2])
        {-^ what to do with values /outside/ @sd@ -} ->
    [BISTR.BisectionTree box varid d v1] ->
    [BISTR.BisectionTree box varid d v2]
zipOnSubdomain valSplitter ix maxDepth sdomB updateInside updateTouch updateAway bistrs =
    resultBistrs
    where
    resultBistrs = 
        zz $ BISTR.syncMany valSplitter ix bistrs
    zz bistrs@(BISTR.Leaf depth domB _ : _) 
        | intersect = 
            case depth < maxDepth of
                True ->
                    zz $ map (BISTR.split valSplitter ix var pt domB) bistrs  
                False ->
                    error "BTINTEG: zipOnSubdomain: maxDepth reached but irregular splitting not implemented yet"
        | away = lift updateAway
        | touch = lift updateTouch
        | inside = lift updateInside
        where
        (var, (_,pt)) = DBox.bestSplit domB
        lift updateFn =
            map (BISTR.Leaf depth domB) $ 
                updateFn domB $ 
                    map BISTR.bistrVal bistrs
        (away, touch, intersect, inside) =
            DBox.classifyPosition domB sdomB
    zz bistrs@(BISTR.Node depth domB var pt _ _ : _) =
        zipWith 
            (\bLO bHI -> BISTR.Node depth domB var pt bLO bHI) 
            (zz $ map BISTR.bistrLO bistrs) 
            (zz $ map BISTR.bistrHI bistrs) 

