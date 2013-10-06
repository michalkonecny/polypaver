{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.Approx.IntervalOI
    Description :  arbitrary precision outer/inner function enclosures on @[-1,1]^n@
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A construction of an outer/inner enclosure of a real function on
    the domain [-1,1]^n for some n using elements of some
    base (eg rational functions or polynomials).
-}
module Numeric.ER.RnToRm.UnitDom.Approx.IntervalOI 
(
    ERFnIntervalOI(..)
)
where

import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.Approx.OI
import Numeric.ER.Real.Arithmetic.Elementary

import Numeric.ER.RnToRm.UnitDom.Approx.Interval (sincos)

import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import qualified Numeric.ER.RnToRm.UnitDom.Base as UFB 
import Numeric.ER.RnToRm.UnitDom.Base ((+^),(-^),(*^),multiplyEncl)
import Numeric.ER.RnToRm.UnitDom.Approx.Interval

import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL

import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox, DomainIntBox)
import Numeric.ER.BasicTypes

import Numeric.ER.Misc

import Numeric.ER.ShowHTML
import qualified Text.Html as H

import qualified Data.Map as Map

import Data.Typeable
import Data.Generics.Basics
import Data.Binary

{- only for testing in ghci, to be removed: -}
--import Numeric.ER.Real.DefaultRepr
--import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.Polynom
--import Numeric.ER.RnToRm.UnitDom.ChebyshevBase.PolynomBase
--type FAPU = ERFnInterval (ERChebPoly (Box Int) B)
--fapuConst1 = (UFA.const 0 [1]) :: FAPU
--fapuConst2 = (UFA.const 0 [2]) :: FAPU
{- end of testing specific code -}

data ERFnIntervalOI fb =
    ERFnIntervalOIAny 
    {
        erfnoiContext :: ERFnContext
    }
    |
    ERFnIntervalOI
    {
--        erfnLowerNeg :: fb,
--        erfnUpper :: fb,
        erfnoiContext :: ERFnContext,
        erfnoiOuter :: (fb, fb), 
        erfnoiInner :: ((fb, fb), Bool) 
--        ,
--        erfnIsDefinitelyConsistent :: Bool,
--        erfnIsDefinitelyAntiConsistent :: Bool
    }
    deriving (Typeable, Data)

instance (Binary a) => Binary (ERFnIntervalOI a) where
  put (ERFnIntervalOIAny a) = putWord8 0 >> put a
  put (ERFnIntervalOI a b c) = putWord8 1 >> put a >> put b >> put c
--  put (ERFnInterval a b c d e) = putWord8 1 >> put a >> put b >> put c >> put d >> put e
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (ERFnIntervalOIAny a)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (ERFnIntervalOI a b c)
--      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (ERFnInterval a b c d e)
      _ -> fail "no parse"
    
instance 
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    Show (ERFnIntervalOI fb)
    where
    show (ERFnIntervalOIAny _) = "ERFnIntervalIOAny"
    show (ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDefinitelyAC)) =
        "\nERFnIntervalOI"
--        ++ " (definitely consistent: " ++ show isC 
--        ++ "anticonsistent: " ++ show isDefinitelyAC ++ ")"        
        ++ "\n  context = " ++ show ctxt
        ++ "\n  outer upper = " ++ ufbShow oh
        ++ "\n  outer lower = " ++ ufbShow (UFB.neg oln)
        ++ "\n  inner upper = " ++ ufbShow ih
        ++ "\n  inner lower = " ++ ufbShow (UFB.neg iln)
        ++ "\n  inner is definitely anticonsistent: " ++ show isDefinitelyAC ++ "\n"
--        ++ "  global = " ++ show gl ++ "\n"
        where
        ufbShow = UFB.showDiGrCmp 10 False False

instance
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    H.HTML (ERFnIntervalOI fb)
--    where
--    toHtml (ERFnIntervalAny ctxt) =
--        H.toHtml "ERFnIntervalAny"
--    toHtml (ERFnInterval ln h ctxt) =
----        H.toHtml $
----            abovesTable
----                [
----                    H.toHtml "ERFnInterval",
--                    H.toHtml $ H.simpleTable [H.border 2] [] 
--                        [
--                            [H.toHtml "upper = ", H.toHtml $ ufbShow h],
--                            [H.toHtml "lower = ", H.toHtml $ ufbShow (UFB.neg ln)]
--                        ]
----                ]
--        where
--        ufbShow = UFB.showDiGrCmp 10 False False
--
instance
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    Eq (ERFnIntervalOI fb)
    where
    (ERFnIntervalOI ctxt1 o1 i1) 
            == (ERFnIntervalOI ctxt2 o2 i2) =
        error "ERFnIntervalIO: equality not implemented"
    _ == _ =
        error "ERFnIntervalIO: equality not implemented"

instance 
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    Ord (ERFnIntervalOI fb) 
    where
    compare 
            (ERFnIntervalOI ctxt1 o1 i1) 
            (ERFnIntervalOI ctxt2 o2 i2) =
        error "ERFnIntervalOI: comparison not implemented; consider leqReals or compareApprox from class ERApprox instead"
    compare _ _ =
        error "ERFnIntervalOI: comparison not implemented; consider leqReals or compareApprox from class ERApprox instead"
    
    
instance 
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, 
     UFB.ERUnitFnBaseIElementary boxb boxra varid b ra fb,
     Show varid, Show boxra) =>
    Num (ERFnIntervalOI fb)
    where
    fromInteger n = UFA.const [fromInteger n]
    negate f@(ERFnIntervalOIAny _) = f
    negate (ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDefinitelyAC)) =
        ERFnIntervalOI ctxt (oh,oln) ((ih,iln),isDefinitelyAC)
    (ERFnIntervalOI ctxt1 oe1 ie1) + 
        (ERFnIntervalOI ctxt2 oe2 ie2) =
        normalise $
        ERFnIntervalOI ctxt oe ie
        where
        oe = UFB.addEncl maxDegr maxSize oe1 oe2
        ie = UFB.addIEncl maxDegr maxSize ie1 ie2
        maxDegr = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
        ctxt = erfnContextUnify ctxt1 ctxt2
    f1 + f2 = ERFnIntervalOIAny ctxt
        where
        ctxt = erfnContextUnify (erfnoiContext f1) (erfnoiContext f2)
    (ERFnIntervalOI ctxt1 oe1 ie1) * 
        (ERFnIntervalOI ctxt2 oe2 ie2) =
        normalise $
        ERFnIntervalOI ctxt oe ie
        where
        oe = UFB.multiplyEncl maxDegr maxSize oe1 oe2
        ie = UFB.multiplyIEncl maxDegr maxSize ie1 ie2
        maxDegr = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
        ctxt = erfnContextUnify ctxt1 ctxt2
    f1 * f2 = ERFnIntervalOIAny ctxt
        where
        ctxt = erfnContextUnify (erfnoiContext f1) (erfnoiContext f2)

instance 
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, 
     UFB.ERUnitFnBaseIElementary boxb boxra varid b ra fb, 
     Show varid, Show boxra) =>
    Fractional (ERFnIntervalOI fb)
    where
    fromRational r = UFA.const [fromRational r]
    recip f@(ERFnIntervalOIAny _) = f
    recip (ERFnIntervalOI ctxt oe@(oln,oh) ie@((iln,ih),isDAC))
        | certainAboveZero =
            normalise $
            ERFnIntervalOI ctxt oeR posieR
        | certainBelowZero =
            normalise $
            ERFnIntervalOI ctxt oeR negieR
--        | certainNoZero =
--            normalise $
--            ERFnIntervalOI ctxt oeR ieR
        | otherwise = ERFnIntervalOIAny ctxt
        where
--        certainNoZero =
--            certainAboveZero || certainBelowZero
        certainAboveZero =
            certainOuterAboveZero && certainInnerACAboveZero
        certainBelowZero =
             certainOuterBelowZero && certainInnerACBelowZero
        certainOuterAboveZero =
             UFB.upperBound ix oln < 0
        certainInnerACAboveZero =
             UFB.upperBound ix (UFB.neg ih) < 0
        certainOuterBelowZero =         
             UFB.upperBound ix oh < 0 
        certainInnerACBelowZero =         
             UFB.upperBound ix (UFB.neg iln) < 0 
        oeR = 
            UFB.recipEncl maxDegr maxSize ix oe
        posieR = 
            UFB.recipIEnclPositive maxDegr maxSize ix ((iln,ih),isDAC)
        negieR = 
            negIEncl $ 
            UFB.recipIEnclPositive maxDegr maxSize ix $ 
            ((ih,iln),isDAC)
        negIEncl ((a,b),c) = ((b,a),c)
--        hnRecipUp =
--            UFB.recipUp maxDegr maxSize ix (negate h) 
--        lRecipUp =
--            UFB.recipUp maxDegr maxSize ix (negate ln)
        maxDegr = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
        ix = int2effIx $ 3 * maxDegr         

instance
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, UFB.ERUnitFnBaseIElementary boxb boxra varid b ra fb, 
     Show varid, Show boxra) =>
    RA.ERApprox (ERFnIntervalOI fb) 
    where
    initialiseBaseArithmetic _ =
        UFB.initialiseBaseArithmetic (UFB.const 0 :: fb)
--    getGranularity (ERFnIntervalOIAny ctxt) = erfnCoeffGranularity ctxt
--    getGranularity (ERFnIntervalOI ctxt (oln,oh) ((iln,ih),_)) =
--        maximum $ 
--            erfnCoeffGranularity ctxt : map UFB.getGranularity [oln,oh,iln,ih]
--    setGranularity gran (ERFnIntervalAny ctxt) = 
--        ERFnIntervalAny $ ctxt { erfnCoeffGranularity = gran }
--    setGranularity gran (ERFnInterval ln h ctxt) =
--        ERFnInterval 
--            (UFB.setGranularity gran ln) (UFB.setGranularity gran h) 
--            (ctxt { erfnCoeffGranularity = gran })
--    setMinGranularity gran (ERFnIntervalAny ctxt) = 
--        ERFnIntervalAny
--            (ctxt { erfnCoeffGranularity = max gran (erfnCoeffGranularity ctxt) })
--    setMinGranularity gran (ERFnInterval ln h ctxt) =
--        ERFnInterval 
--            (UFB.setMinGranularity gran ln) (UFB.setMinGranularity gran h) 
--            (ctxt { erfnCoeffGranularity = max gran (erfnCoeffGranularity ctxt) })
----    getPrecision (ERFnIntervalAny _) = 0
----    getPrecision f = intLog 2 (1 + (fst $ RA.integerBounds (FA.volume f))) -- wrong! 
    isBottom (ERFnIntervalOIAny _) = True
    isBottom _ = False
--    isConsistent (ERFnIntervalOIAny _) = True
--    isConsistent (ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDAC)) = ...
--    f1@(ERFnInterval ln1 h1 ctxt1) /\ f2@(ERFnInterval ln2 h2 ctxt2) =
------ #ifdef RUNTIME_CHECKS
------         check ("ERFnInterval: /\\:\n f1:\n" ++ show f1 ++ " f2:\n" ++ show f2 ++ "\n result:\n") $
------ #endif
--        normalise $
--        ERFnInterval 
--            (UFB.minUp maxDegr maxSize ln1 ln2) 
--            (UFB.minUp maxDegr maxSize h1 h2) 
--            ctxt
--        where
--        ctxt = erfnContextUnify ctxt1 ctxt2
--        maxDegr = erfnMaxDegree ctxt
--        maxSize = erfnMaxSize ctxt
--    (ERFnIntervalAny ctxt1) /\ (ERFnInterval ln2 h2 ctxt2) =
--        ERFnInterval ln2 h2 ctxt
--        where
--        ctxt = erfnContextUnify ctxt1 ctxt2
--    (ERFnInterval ln1 h1 ctxt1) /\ (ERFnIntervalAny ctxt2) =
--        ERFnInterval ln1 h1 ctxt
--        where
--        ctxt = erfnContextUnify ctxt1 ctxt2
--    f1 /\ f2 = ERFnIntervalAny ctxt
--        where
--        ctxt = erfnContextUnify (erfnContext f1) (erfnContext f2)
    leqReals f1 f2 = 
--        unsafePrint ("ERInterval: leqReals: sizes: " ++ show (FA.getSize f1) ++ ", " ++ show (FA.getSize f2)) $ 
        erfnintLeq f1 f2
    {-
        The relation 'refines' corresponds to enclosure inclusion of the outer 
        enclosure of the left argument in the inner enclosure of the right 
        argument. Probably wrong way to implement this... should split into
        refinesI and refinesO? 
    -}
--    refines _ (ERFnIntervalOIAny _) = True
--    refines (ERFnIntervalOIAny _) _ = False
--    refines (ERFnIntervalOI _ (oln,oh) _) (ERFnIntervalOI _ _ ((iln,ih),_)) = 
--        (UFB.upperBound 10 (iln -^ oln) >= 0)
--        &&
--        (UFB.upperBound 10 (ih -^ oh) >= 0)
--    compareApprox (ERFnIntervalAny _) (ERFnIntervalAny _) = EQ
--    compareApprox (ERFnIntervalAny _) _ = LT
--    compareApprox _ (ERFnIntervalAny _) = GT
--    compareApprox (ERFnInterval ln1 h1 _) (ERFnInterval ln2 h2 _) =
--        compareComposeMany
--        [
--            UFB.compareApprox h1 h2,
--            UFB.compareApprox ln1 ln2
--        ]
--
erfnintLeq left right
    | left `isClearlyBelow` right = Just True
    | right `isClearlyStrictlyBelow` left = Just False
    | otherwise = Nothing
    where
    isClearlyBelow (ERFnIntervalOIAny _) _ = False
    isClearlyBelow _ (ERFnIntervalOIAny _) = False
    isClearlyBelow 
        f@(ERFnIntervalOI _ (_,ohf) _) 
        g@(ERFnIntervalOI _ (olng,_) _)
--        | UFB.upperBoundPrecise 10 (ohf +^ olng) <= 0 = True
        | UFB.upperBound 10 (ohf +^ olng) <= 0 = True
        | otherwise = False
    isClearlyStrictlyBelow (ERFnIntervalOIAny _) _ = False
    isClearlyStrictlyBelow _ (ERFnIntervalOIAny _) = False
    isClearlyStrictlyBelow
        f@(ERFnIntervalOI _ (_,ohf) _) 
        g@(ERFnIntervalOI _ (olng,_) _)
--        | UFB.upperBoundPrecise 10 (ohf +^ olng) < 0 = True    
        | UFB.upperBound 10 (ohf +^ olng) < 0 = True    
        | otherwise = False

instance
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb,
     UFB.ERUnitFnBaseIElementary boxb boxra varid b ra fb,
     Show varid, Show boxra) =>
    RA.ERIntApprox (ERFnIntervalOI fb) 
    where
    doubleBounds (ERFnIntervalOIAny _) = (-(1/0), 1/0)
    doubleBounds (ERFnIntervalOI ctxt oe@(oln,oh) _) =
--        (negate $  B.toDouble $ UFB.upperBoundPrecise 10 oln, 
--         B.toDouble $ UFB.upperBoundPrecise 10 oh) 
        (negate $  B.toDouble $ UFB.upperBound 10 oln, 
         B.toDouble $ UFB.upperBound 10 oh) 
----    floatBounds :: ira -> (Float, Float)
----    integerBounds :: ira -> (ExtendedInteger, ExtendedInteger)
--    bisectDomain maybePt (ERFnIntervalAny c) =
--        error "ERFnInterval: RA.bisectDomain: cannot bisect ERFnIntervalAny"
--    bisectDomain maybePt (ERFnInterval ln h c) =
--        (ERFnInterval ln midUp c,
--         ERFnInterval midDownNeg h c)
--         where
--         (midDownNeg, midUp) =
--            case maybePt of
--                Nothing ->
--                    (UFB.scaleUp (1/2) $ ln -^ h, UFB.scaleUp (1/2) $ h -^ ln)
--                Just (ERFnInterval lnPt hPt _) ->
--                    (lnPt, hPt)
--    bounds (ERFnIntervalAny c) =
--        error "ERFnInterval: RA.bounds: cannot get bounds for ERFnIntervalAny"
--    bounds (ERFnInterval ln h c) =
--        (ERFnInterval ln (UFB.neg ln) c,
--         ERFnInterval (UFB.neg h) h c) 
    f1@(ERFnIntervalOI ctxt1 oe1@(oln1,oh1) ie1@((iln1,ih1),isDAC1)) \/ 
        f2@(ERFnIntervalOI ctxt2 oe2@(oln2,oh2) ie2@((iln2,ih2),isDAC2)) =
---- #ifdef RUNTIME_CHECKS
----         check ("ERFnInterval: abs:\n f1:\n" ++ show f1 ++ " f2:\n" ++ show f2 ++ "\n result:\n") $
---- #endif
        normalise $
        ERFnIntervalOI ctxt oe ie
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
        oe = (oln,oh)
        oln = UFB.maxUp maxDegree maxSize oln1 oln2
        oh = UFB.maxUp maxDegree maxSize oh1 oh2
        ie = ((iln,ih),isDAC)
        iln = UFB.maxDown maxDegree maxSize iln1 iln2
        ih = UFB.maxDown maxDegree maxSize ih1 ih2
        {-^
            Note that using maxDown here is safe, but very wasteful. It should
            be possible to find a safe yet more precise way of computing this
            type of min/max for bound functions...
        -}
        isDAC = False
        maxDegree = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
    (ERFnIntervalOIAny ctxt1) \/ (ERFnIntervalOI ctxt2 _ _) =
        ERFnIntervalOIAny ctxt
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    (ERFnIntervalOI ctxt1 _ _) \/ (ERFnIntervalOIAny ctxt2) =
        ERFnIntervalOIAny ctxt
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    f1 \/ f2 = ERFnIntervalOIAny ctxt
        where
        ctxt = erfnContextUnify (erfnoiContext f1) (erfnoiContext f2)

instance
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, 
     UFB.ERUnitFnBaseIElementary boxb boxra varid b ra fb,
     RAEL.ERApproxElementary ra, 
     Show varid, Show boxra) =>
    RAEL.ERApproxElementary (ERFnIntervalOI fb) 
    where
    abs ix f@(ERFnIntervalOIAny _) = f
    abs ix f@(ERFnIntervalOI ctxt oe@(oln,oh) ie@((iln,ih),isDAC)) =
---- #ifdef RUNTIME_CHECKS
----         check ("ERFnIntervalOI: abs:\n f:\n" ++ show f ++ "\n result:\n") $
---- #endif
        normalise $
        ERFnIntervalOI ctxt (minoholn0Up, maxoholnUp) ((minihiln0Dn, maxihilnDn), isDAC) 
        where
        maxoholnUp = UFB.maxUp maxDegree maxSize oh oln 
        minoholn0Up =
            UFB.minUp maxDegree maxSize (UFB.const 0) $
                UFB.minUp maxDegree maxSize oh oln
        maxihilnDn = 
            UFB.neg $ UFB.minUp maxDegree maxSize ihn il -- ie maxDn 
        minihiln0Dn =
            UFB.neg $
                UFB.maxUp maxDegree maxSize (UFB.const 0) $ -- ie minDn
                    UFB.maxUp maxDegree maxSize ihn il -- ie minDn
        ihn = UFB.neg ih
        il = UFB.neg iln
        maxDegree = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
    sqrt ix f@(ERFnIntervalOIAny _) = f
    sqrt ix f@(ERFnIntervalOI ctxt oe@(oln,_) ie@((iln,ih),_)) =
--        | certainAboveZero =
            normalise $
            ERFnIntervalOI ctxt oeR ieR
--        | otherwise = ERFnIntervalOIAny ctxt
        where
--        certainAboveZero = -- OK since consistent inner will be inside outer
--            certainOuterAboveZero && certainInnerACAboveZero
--        certainOuterAboveZero =
--             UFB.upperBound ix oln < 0
--        certainInnerACAboveZero =
--             UFB.upperBound ix (UFB.neg ih) < 0
--             &&
--             UFB.upperBound ix iln < 0
        oeR = 
            UFB.sqrtEncl maxDegr maxSize ix oe
        ieR = 
            UFB.sqrtIEncl maxDegr maxSize ix ie
        maxDegr = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
    exp ix f@(ERFnIntervalOIAny _) = f
    exp ix f@(ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDAC)) = 
        normalise $
        ERFnIntervalOI ctxt oeExp ieExp
        where
        maxDegree = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
--        (olExpNeg, ohExp) =
        oeExp =
            case (UFB.upperBound ix (oh +^ oln) <= 1) of
                True -> 
                    UFB.expEncl maxDegree maxSize ix (oln, oh)
                False ->
                    (olExpNeg, ohExp)
                    where
                    (olExpNeg, _) = UFB.expEncl maxDegree maxSize ix (oln, UFB.neg oln)
                    (_, ohExp) = UFB.expEncl maxDegree maxSize ix (UFB.neg oh,oh)
--        ((ilExpNeg, ihExp), _) =
        ieExp =
            case (UFB.upperBound ix (ih +^ iln) <= 1) of
                True -> 
                    UFB.expIEncl maxDegree maxSize ix ((iln, ih),isDAC)
                False ->
                    ((ilExpNeg, ihExp),isDAC)
                    where
                    ilExpNeg = UFB.neg ilExp
                    ihExp = UFB.neg ihExpNeg
                    ((_, ilExp), _) = UFB.expIEncl maxDegree maxSize ix ((iln, UFB.neg iln),False)
                    ((ihExpNeg, _), _) = UFB.expIEncl maxDegree maxSize ix ((UFB.neg ih,ih),False)
    sin ix f@(ERFnIntervalOIAny c) = 
        ERFnIntervalOI c (one, one) ((mone, mone),True)
        where
        one = UFB.const 1
        mone = UFB.neg one
    sin ix f@(ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDAC)) = 
--        unsafePrint
--        (
--            "ERFnInterval: RAEL.sin: "
--            ++ "\n h = " ++ show h
--            ++ "\n ln = " ++ show ln
--            ++ "\n hSin = " ++ show hSin
--            ++ "\n lSinNeg = " ++ show lSinNeg
--        ) $
---- #ifdef RUNTIME_CHECKS
----        check ("ERFnInterval: sin:\n f:\n" ++ show f ++ "\n result:\n") $
---- #endif
        -- normalise $ -- enable this when inner rounding is implemented
        ERFnIntervalOI ctxt (olSinNeg, ohSin) ((ilSinNeg, ihSin), isDAC)
        where
        (olSinNeg, ohSin) = sincos True maxDegree maxSize ix (oln, oh)
        (ilSinNeg, ihSin) =(mone, mone)
            where 
            one = UFB.const 1
            mone = UFB.neg one 
--        error "sincosInner not implemented yet" --sincosInner True maxDegree maxSize ix (iln, ih)
        maxDegree = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
    cos ix f@(ERFnIntervalOIAny c) =
        ERFnIntervalOI c (one, one) ((mone, mone),True)
        where
        one = UFB.const 1
        mone = UFB.neg one
    cos ix f@(ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDAC)) =
--        unsafePrint
--        (
--            "ERFnInterval: RAEL.cos: "
--            ++ "\n h = " ++ show h
--            ++ "\n ln = " ++ show ln
--            ++ "\n uCos = " ++ show uCos
--            ++ "\n lCosNeg = " ++ show lCosNeg
--        ) $
        -- normalise $ -- enable this when inner rounding is implemented
        ERFnIntervalOI ctxt (olCosNeg, ohCos) ((ilCosNeg, ihCos), isDAC)
        where
        (olCosNeg, ohCos) = sincos False maxDegree maxSize ix (oln, oh)
        (ilCosNeg, ihCos) = (mone, mone)
            where 
            one = UFB.const 1
            mone = UFB.neg one
--            error "sincosInner not implemented yet" --sincosInner False maxDegree maxSize ix (iln, ih)
        maxDegree = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
--    atan ix f@(ERFnIntervalAny c) =
--        ERFnInterval one one c
--        where
--        one = UFB.const 1
--    atan ix f@(ERFnInterval ln h c) =
----        unsafePrint
----        (
----            "ERFnInterval: RAEL.atan: "
----            ++ "\n u = " ++ show u
----            ++ "\n ln = " ++ show ln
----            ++ "\n uAtan = " ++ show uAtan
----            ++ "\n lAtanNeg = " ++ show lAtanNeg
----        ) $
--        normalise $
--        ERFnInterval lAtanNeg hAtan c
--        where
--        maxDegree = erfnMaxDegree c
--        maxSize = erfnMaxSize c
----        ix = int2effIx maxDegree
--        (lAtanNeg, hAtan) = 
--            case (UFB.upperBound ix (h +^ ln) <= 1) of
--                True ->
--                    UFB.atanEncl maxDegree maxSize ix (ln, h)
--                False ->
--                    (lAtanNeg, hAtan)
--                    where
--                    (lAtanNeg, _) = UFB.atanEncl maxDegree maxSize ix (ln, UFB.neg ln)
--                    (_, hAtan) = UFB.atanEncl maxDegree maxSize ix (UFB.neg h,h)
--
--sincos ::
--    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, RAEL.ERApproxElementary ra, RealFrac b) =>
--    Bool {-^ True iff sine, False iff cosine -} -> 
--    Int {-^ maximum representation degree -} -> 
--    Int {-^ maximum approx size -} -> 
--    EffortIndex {-^ how hard to try to eliminate truncation errors -} -> 
--    (fb, fb) ->
--    (fb, fb)
--sincos isSine maxDegree maxSize ix (ln,h)
--    -- p - 2k*pi range within [-pi/2, pi/2]: 
--    | ranfNear0 `RA.refines` plusMinusPiHalf =
----        unsafePrint
----        (
----            "ERFnInterval: sincos: [-pi/2, pi/2]: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        case isSine of
--            True -> sineShifted (- k2pi)
--            False -> cosineShifted (- k2pi)
--    -- p - 2k*pi range within [0, pi]: 
--    | (ranfNear0 - piHalf) `RA.refines` plusMinusPiHalf =
----        unsafePrint
----        (
----            "ERFnInterval: sincos: [0, pi]: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        case isSine of
--            -- use sin(x) = cos(x - pi/2) and cos(x) = - sin(x - pi/2):
--            True -> cosineShifted (- k2pi - piHalf)
--            False -> sineShiftedNegated (- k2pi - piHalf)
--    -- p - 2k*pi range within [-pi, 0]: 
--    | (ranfNear0 + piHalf) `RA.refines` plusMinusPiHalf =
----        unsafePrint
----        (
----            "ERFnInterval: sincos: [-pi, 0]: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        case isSine of
--            -- use sin(x) = - cos(x + pi/2) and cos(x) = sin(x + pi/2):
--            True -> cosineShiftedNegated (-k2pi + piHalf)
--            False -> sineShifted (-k2pi + piHalf)
--    -- p - 2k*pi range within [pi/2, 3pi/2]: 
--    | (ranfNear0 - pi) `RA.refines` plusMinusPiHalf =
----        unsafePrint
----        (
----            "ERFnInterval: sincos: [pi/2, 3pi/2]: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        -- use sin(x) = - sin(x - pi) and cos(x) = - cos(x - pi)
--        case isSine of
--            True -> sineShiftedNegated (- k2pi - pi)
--            False -> cosineShiftedNegated (- k2pi - pi)
--    | otherwise = 
----        unsafePrint
----        (
----            "ERFnInterval: sincos: big range: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        (UFB.const (-1), UFB.const 1)
----    (expDownwards, expUpwards + valueAtRDnNeg + (UFB.const expRUp))
--    where
----    l = UFB.neg ln
--    ranfNear0 = ranf - k2pi
--    k2pi = k * 2 * pi
--    plusMinusPiHalf = (-piHalfLO) RA.\/ piHalfLO
--    pi = RAEL.pi ix  
--    piHalf = pi / 2
--    (piHalfLO, piHalfHI) = RA.bounds piHalf
--    ranf = 
--        ERInterval 
--            (negate $ UFB.upperBound 10 ln) 
--            (UFB.upperBound 10 h)
--    k = fromInteger $ toInteger kEI
--    (kEI,_) = RA.integerBounds $ 0.5 + (ranf / (2*pi))
--
--    sineShiftedNegated shift =
--        boundsNegate $ sineShifted shift
--        
--    cosineShiftedNegated shift =
--        boundsNegate $ cosineShifted shift
--
--    boundsNegate (pLONeg, pHI) = (pHI, pLONeg)
--        
--    sineShifted shift = -- moving to domain where sinus is non-decreasing
--        case (UFB.upperBound ix (h +^ ln) <= 0.25) of
--            True -> 
--                UFB.sinEncl maxDegree maxSize ix (lnShifted, hShifted)
--            False ->
--                (lSinNeg, hSin)
--                where
--                (lSinNeg, _) = UFB.sinEncl maxDegree maxSize ix (ln, UFB.neg ln)
--                (_, hSin) = UFB.sinEncl maxDegree maxSize ix (UFB.neg h,h)
--        where
--        lnShifted = ln +^ (UFB.const (- shiftLOB))
--        hShifted = h +^ (UFB.const shiftHIB)
--        ERInterval shiftLOB shiftHIB = shift
--
--
--    
--    cosineShifted shift = -- moving to domain where cosinus is non-decreasing
--        case (UFB.upperBound ix (h +^ ln) <= 0.25) of
--            True -> 
--                UFB.cosEncl maxDegree maxSize ix (lnShifted, hShifted)
--            False ->
--                (UFB.minUp maxDegree maxSize lCosDownNeg hCosDownNeg,
--                 UFB.maxUp maxDegree maxSize lCosUp hCosUp 
--                    +^ (UFB.scaleUp 0.5 (h +^ ln))) 
--                        -- this term is important when enclosure hits 0;
--                        -- without it, the result could miss cosine's maximum at 0
--        where
--        (lCosDownNeg, lCosUp) = UFB.cosEncl maxDegree maxSize ix (ln, UFB.neg ln)
--        (hCosDownNeg, hCosUp) = UFB.cosEncl maxDegree maxSize ix (UFB.neg h,h)
--        lnShifted = ln +^ (UFB.const (- shiftLOB))
--        hShifted = h +^ (UFB.const shiftHIB)
--        ERInterval shiftLOB shiftHIB = shift
--    
--    boundsAddErr errB (pLONeg, pHI) =
--        (pLONeg +^ errPoly, pHI +^ errPoly)
--        where
--        errPoly = UFB.const errB

normalise f@(ERFnIntervalOIAny _) = f
normalise f@(ERFnIntervalOI ctxt (oln,oh) ((iln,ih),_))
    | UFB.isValid oh && UFB.isValid oln && UFB.isValid ih && UFB.isValid iln = f
    | otherwise = ERFnIntervalOIAny ctxt
    
--check callerLocation f@(ERFnIntervalAny c) = f
--check callerLocation f@(ERFnInterval ln h c) =
--    ERFnInterval 
--        (UFB.check (callerLocation ++ "upper: ") h) 
--        (UFB.check (callerLocation ++ "neg lower: ") ln) 
--        c
--
--
instance 
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, 
     UFB.ERUnitFnBaseIElementary boxb boxra varid b ra fb,
    Show varid, Show boxra) =>
    FA.ERFnApprox boxra varid ra ra (ERFnIntervalOI fb)
    where
--    check = check
    domra2ranra _ = id
    ranra2domra _ = id
    getMaxDegree (ERFnIntervalOIAny ctxt) =
        erfnMaxDegree ctxt
    getMaxDegree (ERFnIntervalOI ctxt _ _) =
        erfnMaxDegree ctxt
    setMaxDegree maxDegr (ERFnIntervalOIAny c) =
        ERFnIntervalOIAny (c { erfnMaxDegree = maxDegr } )
    setMaxDegree maxDegr (ERFnIntervalOI ctxt oe@(oln,oh) ie@((iln,ih),isDAC)) =
        ERFnIntervalOI 
            (ctxt { erfnMaxDegree = maxDegr } )
            (UFB.reduceDegreeUp maxDegr oln, UFB.reduceDegreeUp maxDegr oh)
            ((UFB.neg $ UFB.reduceDegreeUp maxDegr (UFB.neg iln),
              UFB.neg $ UFB.reduceDegreeUp maxDegr (UFB.neg ih)),isDAC)
--    getSize (ERFnIntervalAny c) = 0
--    getSize (ERFnInterval ln h c) =
--        max (UFB.getSize ln) (UFB.getSize h)
--    getMaxSize (ERFnIntervalOIAny ctxt) =
--        erfnMaxSize ctxt
--    getMaxSize (ERFnIntervalOI ctxt _ _) =
--        erfnMaxSize ctxt
    setMaxSize maxSize (ERFnIntervalOIAny ctxt) =
        ERFnIntervalOIAny (ctxt { erfnMaxSize = maxSize } )
    setMaxSize maxSize (ERFnIntervalOI ctxt oe@(oln,oh) ie@((iln,ih),isDAC)) =
        ERFnIntervalOI 
            (ctxt { erfnMaxSize = maxSize } )
            (UFB.reduceSizeUp maxSize oln, UFB.reduceSizeUp maxSize oh)
            ((UFB.neg $ UFB.reduceSizeUp maxSize (UFB.neg iln),
              UFB.neg $ UFB.reduceSizeUp maxSize (UFB.neg ih)),isDAC)
--    getVariables (ERFnIntervalAny _) = []
--    getVariables (ERFnInterval ln h _) = UFB.getVariables h 
    getRangeApprox (ERFnIntervalOIAny _) = 
        [RA.bottomApprox] 
    getRangeApprox (ERFnIntervalOI ctxt (oln,oh) _) =
        [UFB.raFromEndpoints oh
        (
         (- (UFB.upperBound 10 oln))
        ,
         (UFB.upperBound 10 oh)
        )]            
    scale ratio f@(ERFnIntervalOIAny ctxt) = 
        f
    scale ratio f@(ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDAC)) =
---- #ifdef RUNTIME_CHECKS
----         FA.check ("ERFnInterval: scale:\n before:\n" ++ show f ++ "\n after:\n") $
---- #endif
        normalise $
        case RA.compareReals ratio 0 of
            Just GT -> 
                ERFnIntervalOI 
                    ctxt 
                    (scaleUp ratio oln, scaleUp ratio oh) 
                    ((UFB.neg $ scaleUp ratio $ UFB.neg iln, UFB.neg $ scaleUp ratio $ UFB.neg ih),isDAC)
            Just LT -> 
                ERFnIntervalOI 
                    ctxt 
                    (scaleUp (- ratio) oh, scaleUp (- ratio) oln)
                    ((UFB.neg $ scaleUp (- ratio) $ UFB.neg oh, UFB.neg $ scaleUp (- ratio) $ UFB.neg oln),isDAC)
            _ -> 
                (UFA.const [ratio]) * f
        where
        scaleUp = UFB.scaleApproxUp maxDegree maxSize
        maxDegree = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
    eval ptBox (ERFnIntervalOIAny _) = [RA.bottomApprox]
    eval ptBox (ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDAC)) =
        [RA.fromBounds (lo, up)]
        where
        up = snd $ RA.bounds $ UFB.evalApprox ptBox oh
        lo = fst $ RA.bounds $ negate $ UFB.evalApprox ptBox oln
    partialEval substitutions f@(ERFnIntervalOIAny _) = f
    partialEval substitutions f@(ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDAC)) =
        normalise $
        ERFnIntervalOI ctxt (olnP,ohP) ((ilnP,ihP),isDAC)
        where
        ohP = UFB.partialEvalApproxUp substitutions oh
        olnP = UFB.partialEvalApproxUp substitutions oln
        ilnP = UFB.neg $ UFB.partialEvalApproxUp substitutions $ UFB.neg iln
        ihP = UFB.neg $ UFB.partialEvalApproxUp substitutions $ UFB.neg ih
--    composeNonDecreasing
--            fOuter@(ERFnInterval lnOuter hOuter cOuter)
--            varid
--            fInner@(ERFnInterval lnInner hInner cInner) =
----        unsafePrintReturn
----        (
----            "ER.RnToRm.UnitDom.Interval: composeNonDecreasing: "
----            ++ "\n fOuter = " ++ show fOuter
----            ++ "\n varid = " ++ show varid
----            ++ "\n fInner = " ++ show fInner
----            ++ "\n inconsistencies = " ++ show (UFA.keyPointsConsistencyCheck resultReals result)
----            ++ "\n result = "
----        )
----        $
--        result
--        where
--        resultReals ptB = -- this is only used for consistency checking...
--            (\[x] -> x) $ FA.eval ptBOuter fOuter
--            where
--            ptBOuter =
--                DBox.insert varid fInnerVal ptB
--            fInnerVal =
--                FA.ranra2domra fInner $
--                (\[x] -> x) $ FA.eval ptB fInner
--                
--        result = ERFnInterval ln h c
--        h =
--            erfnUpper $ 
--                UFA.composeWithThin fOuter $
--                    Map.singleton varid
--                    (ERFnInterval (UFB.neg hInner) hInner cInner)
--        ln =
--            erfnLowerNeg $
--                UFA.composeWithThin fOuter $
--                    Map.singleton varid $
--                    (ERFnInterval lnInner (UFB.neg lnInner) cInner)
--        c = erfnContextUnify cOuter cInner
--        
--    composeNonDecreasing fOuter varid fInner = 
--        ERFnIntervalAny c
--        where
--        c = erfnContextUnify (erfnContext fOuter) (erfnContext fInner)
--
instance 
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb,
     UFB.ERUnitFnBaseIElementary boxb boxra varid b ra fb,
     Show varid, Show boxra) =>
    UFA.ERUnitFnApprox boxra varid ra ra (ERFnIntervalOI fb)
    where
    bottomApprox =
        ERFnIntervalOIAny erfnContextDefault
      {- 
        Can't get 'const' through the type checker, even when adding the
        suggested declaration... why doesn't the trick used for ERFnInterval
        work here?
      -}
    const [val] 
        | RA.isBounded val =
---- #ifdef RUNTIME_CHECKS
----             check ("ERFnInterval: const:\n") $
---- #endif
            normalise $
            ERFnIntervalOI ctxt oe ie
--            ERFnIntervalOI
--            {
--                erfnoiContext = ctxt,
--                erfnoiOuter = oe, 
--                erfnoiInner = ie 
--            }
        | otherwise =
            ERFnIntervalOIAny ctxt 
        where
        oe@(_,h) = UFB.constEncl valEndpoints
        ie = UFB.constIEncl valEndpoints
        valEndpoints = UFB.raEndpoints h val
        ctxt = 
            erfnContextDefault
            {
                erfnCoeffGranularity = RA.getGranularity val
            }
    affine [val] coeffsSingletons
        | RA.isBounded val && (and $ map (RA.isBounded . head) $ Map.elems coeffsSingletons) =
---- #ifdef RUNTIME_CHECKS
----             check ("ERFnInterval: affine:\n") $
---- #endif
            normalise $
            ERFnIntervalOI ctxt oe ie
        | otherwise =
            ERFnIntervalOIAny ctxt
        where
        ctxt = 
            erfnContextDefault
            {
                erfnCoeffGranularity = coeffGranularity
            }
        coeffGranularity =
            Map.fold max (RA.getGranularity val) (Map.map RA.getGranularity coeffs)
        coeffs = Map.map (\[a] -> a) coeffsSingletons

        oe = (oln, oh)
        oh = UFB.affine (valH + coeffTotalRadius) (Map.map fst coeffsMidsAndErrbnds)
        oln = UFB.affine (coeffTotalRadius - valL) (Map.map (negate . fst) coeffsMidsAndErrbnds)
        
        ie = ((iln, ih), True)
        ih = 
            UFB.affine 
                (valH `plusDown` (- coeffTotalErrbnd)) 
                (Map.map fst coeffsMidsAndErrbnds)
        iln = 
            UFB.affine 
                (negate $ coeffTotalErrbnd + valL) 
                (Map.map (negate . fst) coeffsMidsAndErrbnds)
        coeffTotalErrbnd =
            Map.fold (+) 0 $ Map.map snd coeffsMidsAndErrbnds
        
        (valL, valH) = UFB.raEndpoints oh val
        
        coeffTotalRadius = 
            Map.fold (+) 0 coeffsRads
        coeffsRads =
            Map.map (\(l,h) -> (h - l)/2) coeffsEndpoints
        coeffsEndpoints =
            Map.map
                (mapPairHomog (B.setMinGranularity coeffGranularity) 
                 . 
                 UFB.raEndpoints oh) 
                coeffs
        coeffsMidsAndErrbnds =
            Map.map computeMidCorr coeffsEndpoints
            where
            computeMidCorr (l,h) =
                (midUp, midUp - midDown)
                where
                midUp = (l+h)/2
                midDown = negate $ ((negate l) + (negate h)) / 2
    getAffineUpperBound (ERFnIntervalOIAny _) = ([1/0], Map.empty)
    getAffineUpperBound (ERFnIntervalOI _ (oln,oh) _) =
        (t c, Map.map t coeffs)
        where
        (c, coeffs) = UFB.getAffineUpperBound oh
        t c = [UFB.raFromEndpoints oh (c,c)]
    composeWithThin
            f@(ERFnIntervalOIAny ctxt)
            substitutions =
        f
    composeWithThin
            f@(ERFnIntervalOI ctxt oe@(oln,oh) ie@((iln,ih),isDefinitelyAC))
            substitutions =
--        unsafePrintReturn
--        (
--            "ER.RnToRm.UnitDom.Interval: composeWithThin: "
--            ++ "\n f = " ++ show f
--            ++ "\n substitutions = " ++ show substitutions
--            ++ "\n inconsistencies = " ++ show (UFA.keyPointsConsistencyCheck resultReals result)
--            ++ "\n result = "
--        )
--        $
        result
        where
        resultReals ptB = -- this is only used for consistency checking...
            (\[x] -> x) $
            FA.eval ptBOuter f
            where
            ptBOuter =
                foldl insertVal ptB $ Map.toList substitutions
            insertVal  ptB (varid, fInner) =
                DBox.insert varid (evalPtB fInner) ptB
            evalPtB fInner =
                FA.ranra2domra fInner $ (\[x] -> x) $ FA.eval ptB fInner
                
        result = ERFnIntervalOI ctxt oeNew ieNew
        oeNew = (olnNew, ohNew)
        ieNew = ((ilnNew, ihNew), isDefinitelyAC)
        olnNew = UFB.composeManyUp maxDegree maxSize oln ufbSubstitutions
        ohNew = UFB.composeManyUp maxDegree maxSize oh ufbSubstitutions 
        ilnNew = UFB.composeManyDown maxDegree maxSize iln ufbSubstitutions
        ihNew = UFB.composeManyDown maxDegree maxSize ih ufbSubstitutions 
        ufbSubstitutions = Map.map (snd . erfnoiOuter) substitutions
        maxDegree = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
--        ctxt = erfnContextUnify ctxt1 ctxt2
--    intersectMeasureImprovement ix vars
--            f1@(ERFnIntervalAny ctxt1) 
--            f2@(ERFnIntervalAny ctxt2) =
--        (ERFnIntervalAny ctxt, RA.bottomApprox)
--        where
--        ctxt = erfnContextUnify ctxt1 ctxt2
--    intersectMeasureImprovement ix vars
--            f1@(ERFnIntervalAny ctxt1) 
--            f2@(ERFnInterval ln2 h2 ctxt2) =
--        (ERFnInterval ln2 h2 ctxt, RA.plusInfinity)
--        where
--        ctxt = erfnContextUnify ctxt1 ctxt2
--    intersectMeasureImprovement ix vars
--            f1@(ERFnInterval ln1 h1 ctxt1) 
--            f2@(ERFnIntervalAny ctxt2) = 
--        (ERFnInterval ln1 h1 ctxt, 1)
--        where
--        ctxt = erfnContextUnify ctxt1 ctxt2
--    intersectMeasureImprovement ix vars
--            f1@(ERFnInterval ln1 h1 ctxt1) 
--            f2@(ERFnInterval ln2 h2 ctxt2) =
--        case RA.compareReals improvementRA 1 of
--            Just LT -> (f1, 1) -- intersection made it worse, keep original
--            _ ->  (intersection, improvementRA)
--        where
--        intersection = 
------ #ifdef RUNTIME_CHECKS
------             check ("ERFnInterval: intersectMeasureImprovement:\n f1:\n" ++ show f1 ++ "\n f2:\n" ++ show f2 ++ "\n intersection:\n") $
------ #endif
--            normalise $
--            f1 RA./\ f2
--        improvementRA 
--            | 0 `RA.refines` intersectionVolume && 0 `RA.refines` f1Volume = 1
----                error $ 
----                    "ERFnInterval: intersectMeasureImprovement: inconsistent result: " 
----                    ++ show intersection
--            | otherwise =
--                 f1Volume / intersectionVolume
--        intersectionVolume = UFA.volume vars intersection
--        f1Volume = UFA.volume vars f1
--        ctxt = erfnContextUnify ctxt1 ctxt2
--    volume vars (ERFnIntervalOIAny c) = RA.plusInfinity
--    volume vars (ERFnIntervalOI ctxt (oln,oh) ((iln,ih),_)) =
--        UFB.raFromEndpoints h (volL, volH)
--        where 
--        ovolH = UFB.volumeAboveZeroUp vars (ln +^ h)
--        ovolL = negate $ UFB.volumeAboveZeroUp vars (l +^ hn)
--        ivolH = UFB.volumeAboveZeroUp vars (ln +^ h)
--        l = UFB.neg ln
--        hn = UFB.neg h
    integrate _ f@(ERFnIntervalOIAny ctxt) _ _ _ = f 
    integrate 
            ix fD@(ERFnIntervalOI ctxt (oln,oh) ((iln,ih),isDAC)) x 
            origin fI@(ERFnIntervalOI _ (olnInit,ohInit) ((ilnInit,ihInit),isDACInit)) =
--        unsafePrintReturn
--        (
--            "ERFnInterval: integrate: " 
--            ++ "\n u = " ++ show u
--            ++ "\n ln = " ++ show ln
--            ++ "\n origin = " ++ show origin
--            ++ "\n uInit = " ++ show uInit
--            ++ "\n lnInit = " ++ show lnInit
--            ++ "\n uIuL = " ++ show uIuL
--            ++ "\n uIuU = " ++ show uIuU
--            ++ "\n uIuOriginL = " ++ show uIuOriginL
--            ++ "\n uIuOriginU = " ++ show uIuOriginU
--            ++ "\n lnIuL = " ++ show lnIuL
--            ++ "\n lnIuU = " ++ show lnIuU
--            ++ "\n lnIuOriginL = " ++ show lnIuOriginL
--            ++ "\n lnIuOriginU = " ++ show lnIuOriginU
--            ++ "\n uIov = " ++ show uIov
--            ++ "\n lnIov = " ++ show lnIov
--            ++ "\n result = "
--        )
--        $
---- #ifdef RUNTIME_CHECKS
----         check ("ERFnInterval: integrate:\n fD:\n" ++ show fD ++ "\n fI:\n" ++ show fI ++ "\n result:\n") $
---- #endif
        normalise $
        (ERFnIntervalOI ctxt (olnIov,ohIov) ((ilnIov,ihIov),isDACIov))
        where
--        -- perform raw integration of both bounds:
--        (hIuL, hIuH) = 
----            mapPair (UFB.reduceDegreeDown maxDegree, UFB.reduceDegreeUp maxDegree) $ 
--                UFB.integrate x h
--        (lnIuL, lnIuH) = 
----            mapPair (UFB.reduceDegreeDown maxDegree, UFB.reduceDegreeUp maxDegree) $ 
--                UFB.integrate x ln
        isDACIov =
            isDAC && isDACInit
        -- perform raw integration of both outer bounds:
        (ohIuL, ohIuH) = 
--            mapPair (UFB.reduceDegreeDown maxDegree, UFB.reduceDegreeUp maxDegree) $ 
                UFB.integrate x oh
        (olnIuL, olnIuH) = 
--            mapPair (UFB.reduceDegreeDown maxDegree, UFB.reduceDegreeUp maxDegree) $ 
                UFB.integrate x oln
        -- perform raw integration of both inner bounds:
        (ihIuL, ihIuH) = 
--            mapPair (UFB.reduceDegreeDown maxDegree, UFB.reduceDegreeUp maxDegree) $ 
                UFB.integrate x ih
        (ilnIuL, ilnIuH) = 
--            mapPair (UFB.reduceDegreeDown maxDegree, UFB.reduceDegreeUp maxDegree) $ 
                UFB.integrate x iln
        maxDegree = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
--        -- constrain the raw integrals to the origin:
--        (hIuOriginLNeg, hIuOriginH) =
--            UFB.composeEncl maxDegree maxSize hIuL x originEncl
--        (lnIuOriginLNeg, lnIuOriginH) = 
--            UFB.composeEncl maxDegree maxSize lnIuL x originEncl
--        originEncl = UFB.constEncl $ UFB.raEndpoints oh origin
        -- constrain the raw outer integrals to the origin:
        (ohIuOriginLNeg, ohIuOriginH) =
            UFB.composeEncl maxDegree maxSize ohIuL x oOriginEncl
        (olnIuOriginLNeg, olnIuOriginH) = 
            UFB.composeEncl maxDegree maxSize olnIuL x oOriginEncl
        oOriginEncl = UFB.constEncl $ UFB.raEndpoints oh origin
        -- constrain the raw inner integrals to the origin:
        ((ihIuOriginLNeg, ihIuOriginH), _) =
            UFB.composeIEncl maxDegree maxSize ihIuH x iOriginIEncl -- SHOULD ihIuH be used here?! 
        ((ilnIuOriginLNeg, ilnIuOriginH), _) = 
            UFB.composeIEncl maxDegree maxSize ilnIuH x iOriginIEncl -- SHOULD ilnIuH be used here?!
        iOriginIEncl = UFB.constIEncl $ UFB.raEndpoints ih origin
--        -- adjust the raw integrated functions to enclose the initial condition function:                        
--        hIov = 
--            UFB.reduceSizeUp maxSize $
--                hIuH +^ hInit +^ hIuOriginLNeg +^ (hIuOriginH +^ hIuOriginLNeg)
--        lnIov = 
--            UFB.reduceSizeUp maxSize $
--                lnIuH +^ lnInit +^ lnIuOriginLNeg +^ (lnIuOriginH +^ lnIuOriginLNeg)
        -- adjust the raw outer integrated functions to enclose the initial condition function:                        
        ohIov = 
            UFB.reduceSizeUp maxSize $
                ohIuH +^ ohInit +^ ohIuOriginLNeg +^ (ohIuOriginH +^ ohIuOriginLNeg)
        olnIov = 
            UFB.reduceSizeUp maxSize $
                olnIuH +^ olnInit +^ olnIuOriginLNeg +^ (olnIuOriginH +^ olnIuOriginLNeg)
        -- adjust the raw inner integrated functions to enclose the initial condition function:                        
        ihIov = 
            UFB.reduceSizeUp maxSize $
                UFB.neg $ 
                (UFB.neg ihIuL) +^ (UFB.neg ihInit) -^ (UFB.neg ihIuOriginH)
                +^ ((UFB.neg ihIuOriginH) +^ (UFB.neg ihIuOriginLNeg))
        ilnIov = 
            UFB.reduceSizeUp maxSize $
                UFB.neg $
                (UFB.neg ilnIuL) +^ (UFB.neg ilnInit) -^ (UFB.neg ilnIuOriginH)
                +^ ((UFB.neg ilnIuOriginH) +^ (UFB.neg ilnIuOriginLNeg))


instance
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    RA.ERApproxApprox (ERFnIntervalOI fb)
    where
    safeIncludes _ (ERFnIntervalOIAny _) = False
    safeIncludes (ERFnIntervalOIAny _) _ = True
    safeIncludes f g =
        (UFB.upperBound 10 (olng -^ ilnf) <= 0)
        &&
        (UFB.upperBound 10 (ohg -^ ihf) <= 0)
        where
        (ERFnIntervalOI _ _ ((ilnf,ihf),_)) = f 
        (ERFnIntervalOI _ (olng,ohg) _) = g
    safeNotIncludes _ (ERFnIntervalOIAny _) = True
    safeNotIncludes (ERFnIntervalOIAny _) _ = False
    safeNotIncludes f g =
        (UFB.upperBound 10 (olnf -^ ilng) < 0)
        ||
        (UFB.upperBound 10 (ohf -^ ihg) < 0)
        where
        (ERFnIntervalOI _ (olnf,ohf) _) = f
        (ERFnIntervalOI _ _ ((ilng,ihg),_)) = g
    includes _ (ERFnIntervalOIAny _) = Just False
    includes (ERFnIntervalOIAny _) _ = Just True
    includes f g
        | RA.safeIncludes f g = Just True
        | RA.safeNotIncludes f g = Just False
        | otherwise = Nothing

instance
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    RA.ERIntApproxApprox (ERFnIntervalOI fb)
    where
    oiBounds f@(ERFnIntervalOIAny ctxt) 
        = ((minfty, infty), (infty, minfty))
        where
        infty = ERFnIntervalOI ctxt (minftyB, inftyB) ((minftyB, inftyB), True)
        minfty = ERFnIntervalOI ctxt (inftyB, minftyB) ((inftyB, minftyB), True)
        inftyB = UFB.const (1/0) 
        minftyB = UFB.neg inftyB 
    oiBounds (ERFnIntervalOI ctxt (oln, oh) ((iln, ih), _))
        = ((c ol oln, c oh ohn), (c il iln, c ih ihn))
        where
        c b bn = ERFnIntervalOI ctxt (bn, b) ((bn, b), True)
        ol = UFB.neg oln
        il = UFB.neg iln
        ohn = UFB.neg oh
        ihn = UFB.neg ih
    fromOIBounds ((ol, oh), (il, ih))
        | isAny ol || isAny oh || isAny il || isAny ih = ERFnIntervalOIAny ctxt
        | otherwise = ERFnIntervalOI ctxt (olnT, ohT) ((ilnT, ihT), False)
        where
        ctxt = erfnoiContext ol
        isAny (ERFnIntervalOIAny _) = True
        isAny _ = False
        ERFnIntervalOI _ (olnT, _) _ = ol
        ERFnIntervalOI _    (_, ohT) _ = oh
        ERFnIntervalOI _    _ ((ilnT, _), _) = il
        ERFnIntervalOI _    _ ((_, ihT), _) = ih
         

instance
    (UFB.ERUnitFnBaseEncl boxb boxra varid b ra fb,
     UFB.ERUnitFnBaseIEncl boxb boxra varid b ra fb) 
    =>
    FA.ERFnApproxApprox boxra varid ra (ERApproxOI ra) (ERFnIntervalOI fb)
    where
    evalAA box (ERFnIntervalOIAny _) = 
        [ERApproxOI (RA.bottomApprox) (RA.topApprox)]
    evalAA box (ERFnIntervalOI _ oe ie) =
        [ERApproxOI (UFB.evalEncl box oe) (UFB.evalIEncl box ie)]


-- only renamed sincos, not adapted it to compute inner yet
--sincosInner :: -- only ren
--    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, RAEL.ERApproxElementary ra) =>
--    Bool {-^ True iff sine, False iff cosine -} -> 
--    Int {-^ maximum representation degree -} -> 
--    Int {-^ maximum approx size -} -> 
--    EffortIndex {-^ how hard to try to eliminate truncation errors -} -> 
--    (fb, fb) ->
--    (fb, fb)
--sincosInner isSine maxDegree maxSize ix (ln,h)
--    -- p - 2k*pi range within [-pi/2, pi/2]: 
--    | ranfNear0 `RA.refines` plusMinusPiHalf =
----        unsafePrint
----        (
----            "ERFnInterval: sincos: [-pi/2, pi/2]: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        case isSine of
--            True -> sineShifted (- k2pi)
--            False -> cosineShifted (- k2pi)
--    -- p - 2k*pi range within [0, pi]: 
--    | (ranfNear0 - piHalf) `RA.refines` plusMinusPiHalf =
----        unsafePrint
----        (
----            "ERFnInterval: sincos: [0, pi]: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        case isSine of
--            -- use sin(x) = cos(x - pi/2) and cos(x) = - sin(x - pi/2):
--            True -> cosineShifted (- k2pi - piHalf)
--            False -> sineShiftedNegated (- k2pi - piHalf)
--    -- p - 2k*pi range within [-pi, 0]: 
--    | (ranfNear0 + piHalf) `RA.refines` plusMinusPiHalf =
----        unsafePrint
----        (
----            "ERFnInterval: sincos: [-pi, 0]: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        case isSine of
--            -- use sin(x) = - cos(x + pi/2) and cos(x) = sin(x + pi/2):
--            True -> cosineShiftedNegated (-k2pi + piHalf)
--            False -> sineShifted (-k2pi + piHalf)
--    -- p - 2k*pi range within [pi/2, 3pi/2]: 
--    | (ranfNear0 - pi) `RA.refines` plusMinusPiHalf =
----        unsafePrint
----        (
----            "ERFnInterval: sincos: [pi/2, 3pi/2]: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        -- use sin(x) = - sin(x - pi) and cos(x) = - cos(x - pi)
--        case isSine of
--            True -> sineShiftedNegated (- k2pi - pi)
--            False -> cosineShiftedNegated (- k2pi - pi)
--    | otherwise = 
----        unsafePrint
----        (
----            "ERFnInterval: sincos: big range: "
----            ++ "\n u = " ++ show u
----            ++ "\n l = " ++ show l
----            ++ "\n ranf = " ++ show ranf
----            ++ "\n k = " ++ show k
----            ++ "\n ranfNear0 = " ++ show ranfNear0
----        ) $
--        (UFB.const (-1), UFB.const 1)
----    (expDownwards, expUpwards + valueAtRDnNeg + (UFB.const expRUp))
--    where
----    l = UFB.neg ln
--    ranfNear0 = ranf - k2pi
--    k2pi = k * 2 * pi
--    plusMinusPiHalf = (-piHalfLO) RA.\/ piHalfLO
--    pi = RAEL.pi ix  
--    piHalf = pi / 2
--    (piHalfLO, piHalfHI) = RA.bounds piHalf
--    ranf = 
--        ERInterval 
--            (negate $ UFB.upperBound 10 ln) 
--            (UFB.upperBound 10 h)
--    k = fromInteger $ toInteger kEI
--    (kEI,_) = RA.integerBounds $ 0.5 + (ranf / (2*pi))
--
--    sineShiftedNegated shift =
--        boundsNegate $ sineShifted shift
--        
--    cosineShiftedNegated shift =
--        boundsNegate $ cosineShifted shift
--
--    boundsNegate (pLONeg, pHI) = (pHI, pLONeg)
--        
--    sineShifted shift = -- moving to domain where sinus is non-decreasing
--        case (UFB.upperBound ix (h +^ ln) <= 0.25) of
--            True -> 
--                UFB.sinEncl maxDegree maxSize ix (lnShifted, hShifted)
--            False ->
--                (lSinNeg, hSin)
--                where
--                (lSinNeg, _) = UFB.sinEncl maxDegree maxSize ix (ln, UFB.neg ln)
--                (_, hSin) = UFB.sinEncl maxDegree maxSize ix (UFB.neg h,h)
--        where
--        lnShifted = ln +^ (UFB.const (- shiftLOB))
--        hShifted = h +^ (UFB.const shiftHIB)
--        ERInterval shiftLOB shiftHIB = shift
--
--
--    
--    cosineShifted shift = -- moving to domain where cosinus is non-decreasing
--        case (UFB.upperBound ix (h +^ ln) <= 0.25) of
--            True -> 
--                UFB.cosEncl maxDegree maxSize ix (lnShifted, hShifted)
--            False ->
--                (UFB.minUp maxDegree maxSize lCosDownNeg hCosDownNeg,
--                 UFB.maxUp maxDegree maxSize lCosUp hCosUp 
--                    +^ (UFB.scaleUp 0.5 (h +^ ln))) 
--                        -- this term is important when enclosure hits 0;
--                        -- without it, the result could miss cosine's maximum at 0
--        where
--        (lCosDownNeg, lCosUp) = UFB.cosEncl maxDegree maxSize ix (ln, UFB.neg ln)
--        (hCosDownNeg, hCosUp) = UFB.cosEncl maxDegree maxSize ix (UFB.neg h,h)
--        lnShifted = ln +^ (UFB.const (- shiftLOB))
--        hShifted = h +^ (UFB.const shiftHIB)
--        ERInterval shiftLOB shiftHIB = shift
--    
--    boundsAddErr errB (pLONeg, pHI) =
--        (pLONeg +^ errPoly, pHI +^ errPoly)
--        where
--        errPoly = UFB.const errB
