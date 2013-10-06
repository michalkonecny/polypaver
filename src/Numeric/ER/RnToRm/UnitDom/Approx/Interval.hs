{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
    Module      :  Numeric.ER.RnToRm.UnitDom.Approx.Interval
    Description :  arbitrary precision function enclosures on @[-1,1]^n@
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    A construction of an enclosure of a real function on
    the domain [-1,1]^n for some n using elements of some
    base (eg rational functions or polynomials).
-}
module Numeric.ER.RnToRm.UnitDom.Approx.Interval 
(
    ERFnInterval(..),
    ERFnContext(..),
    erfnContextDefault,
    erfnContextUnify,
    sincos
)
where

import qualified Numeric.ER.Real.Base as B
import Numeric.ER.Real.Approx.Interval
import Numeric.ER.Real.Arithmetic.Elementary

import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import qualified Numeric.ER.RnToRm.UnitDom.Base as UFB
import Numeric.ER.RnToRm.UnitDom.Base ((+^),(-^),(*^),multiplyEncl)
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
--type FAPU = ERFnInterval (ERChebPoly B) IRA
--fapuConst1 = (UFA.const 0 [1]) :: FAPU
--fapuConst2 = (UFA.const 0 [2]) :: FAPU
{- end of testing specific code -}

data ERFnInterval fb =
    ERFnIntervalAny 
    {
        erfnContext :: ERFnContext
    }
    |
    ERFnInterval 
    {
        erfnLowerNeg :: fb,
        erfnUpper :: fb,
        erfnContext :: ERFnContext
--        ,
--        erfnIsDefinitelyConsistent :: Bool,
--        erfnIsDefinitelyAntiConsistent :: Bool
    }
    deriving (Typeable, Data)

instance (Binary a) => Binary (ERFnInterval a) where
  put (ERFnIntervalAny a) = putWord8 0 >> put a
  put (ERFnInterval a b c) = putWord8 1 >> put a >> put b >> put c
--  put (ERFnInterval a b c d e) = putWord8 1 >> put a >> put b >> put c >> put d >> put e
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (ERFnIntervalAny a)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (ERFnInterval a b c)
--      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (ERFnInterval a b c d e)
      _ -> fail "no parse"
    

data ERFnContext =
    ERFnContext
    {
        erfnMaxDegree :: Int,
        erfnMaxSize :: Int,
        erfnCoeffGranularity :: Granularity
    }
    deriving (Show, Typeable, Data)
    
instance Binary ERFnContext where
  put (ERFnContext a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (ERFnContext a b c)
    
    
erfnContextDefault =
    ERFnContext
    {
        erfnMaxDegree = 2,
        erfnMaxSize = 20,
        erfnCoeffGranularity = 10
    }
    
erfnContextUnify (ERFnContext dg1 sz1 gr1) (ERFnContext dg2 sz2 gr2) =
    ERFnContext (max dg1 dg2) (max sz1 sz2) (max gr1 gr2)

    
instance 
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    Show (ERFnInterval fb)
    where
    show (ERFnIntervalAny _) = "ERFnIntervalAny"
    show (ERFnInterval ln h ctxt) =
        "\nERFnInterval {" ++ show ctxt ++ "}"
--        ++ " (definitely consistent: " ++ show isC 
--        ++ "anticonsistent: " ++ show isAC ++ ")"
        ++ "\n  upper = " ++ ufbShow h
        ++ "\n  lower = " ++ ufbShow (UFB.neg ln)
--        ++ "  global = " ++ show gl ++ "\n"
--        ++ "  context = " ++ show ctxt ++ "\n"
        where
        ufbShow = UFB.showDiGrCmp 10 False False

instance
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    H.HTML (ERFnInterval fb)
    where
    toHtml (ERFnIntervalAny ctxt) =
        H.toHtml "ERFnIntervalAny"
    toHtml (ERFnInterval ln h ctxt) =
--        H.toHtml $
--            abovesTable
--                [
--                    H.toHtml "ERFnInterval",
                    H.toHtml $ H.simpleTable [H.border 2] [] 
                        [
                            [H.toHtml "upper = ", H.toHtml $ ufbShow h],
                            [H.toHtml "lower = ", H.toHtml $ ufbShow (UFB.neg ln)]
                        ]
--                ]
        where
        ufbShow = UFB.showDiGrCmp 10 False False

instance
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    Eq (ERFnInterval fb)
    where
    (ERFnInterval ln1 h1 ctxt1) 
            == (ERFnInterval ln2 h2 ctxt2) =
        error "ERFnInterval: equality not implemented"
    _ == _ =
        error "ERFnInterval: equality not implemented"

instance 
    (UFB.ERUnitFnBase boxb boxra varid b ra fb) =>
    Ord (ERFnInterval fb) 
    where
    compare 
            (ERFnInterval ln1 h1 ctxt1) 
            (ERFnInterval ln2 h2 ctxt2) =
        error "ERFnInterval: comparison not implemented; consider leqReals or compareApprox from class ERApprox instead"
    compare _ _ =
        error "ERFnInterval: comparison not implemented; consider leqReals or compareApprox from class ERApprox instead"
    
    
instance 
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, Show varid, Show boxra, Show fb) =>
    Num (ERFnInterval fb)
    where
    fromInteger n = UFA.const [fromInteger n]
    negate f@(ERFnIntervalAny _) = f
    negate (ERFnInterval ln h ctxt) =
        (ERFnInterval h ln ctxt)
    (ERFnInterval ln1 h1 ctxt1) + (ERFnInterval ln2 h2 ctxt2) =
        normalise $
        ERFnInterval (reduceSzUp ln) (reduceSzUp h) ctxt
        where
        ln = ln1 +^ ln2
        h = h1 +^ h2
        reduceSzUp = UFB.reduceSizeUp maxSize
        maxSize = erfnMaxSize ctxt
        ctxt = erfnContextUnify ctxt1 ctxt2
    f1 + f2 = ERFnIntervalAny ctxt
        where
        ctxt = erfnContextUnify (erfnContext f1) (erfnContext f2)
    (ERFnInterval ln1 h1 ctxt1) * (ERFnInterval ln2 h2 ctxt2) =
        normalise $
        ERFnInterval ln h ctxt
        where
        (ln, h) = multiplyEncl maxDegr maxSize (ln1, h1) (ln2, h2)
        maxDegr = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
        ctxt = erfnContextUnify ctxt1 ctxt2
    f1 * f2 = ERFnIntervalAny ctxt
        where
        ctxt = erfnContextUnify (erfnContext f1) (erfnContext f2)
        
instance 
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, Show varid, Show boxra, Show fb) =>
    Fractional (ERFnInterval fb)
    where
    fromRational r = UFA.const [fromRational r]
    recip f@(ERFnIntervalAny _) = f
    recip (ERFnInterval ln h ctxt)
        | certainNoZero =
            normalise $
            ERFnInterval lnR hR ctxt
        | otherwise = ERFnIntervalAny ctxt
        where
        (hR, lnR) = UFB.recipEncl maxDegr maxSize ix (h,ln)
        certainNoZero =
            certainAboveZero || certainBelowZero
        certainAboveZero =
             UFB.upperBound ix ln < 0 && UFB.lowerBound ix h > 0
        certainBelowZero =         
             UFB.upperBound ix h < 0 && UFB.lowerBound ix ln > 0
--        hnRecipUp =
--            UFB.recipUp maxDegr maxSize ix (negate h) 
--        lRecipUp =
--            UFB.recipUp maxDegr maxSize ix (negate ln)
        maxDegr = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
        ix = int2effIx $ 3 * maxDegr         

instance
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, Show varid, Show boxra, Show fb) =>
    RA.ERApprox (ERFnInterval fb) 
    where
    initialiseBaseArithmetic _ =
        UFB.initialiseBaseArithmetic (UFB.const 0 :: fb)
    getGranularity (ERFnIntervalAny ctxt) = erfnCoeffGranularity ctxt
    getGranularity (ERFnInterval ln h ctxt) =
        max (erfnCoeffGranularity ctxt) $ 
            max (UFB.getGranularity ln) (UFB.getGranularity h)
    setGranularityOuter gran (ERFnIntervalAny ctxt) = 
        ERFnIntervalAny $ ctxt { erfnCoeffGranularity = gran }
    setGranularityOuter gran (ERFnInterval ln h ctxt) =
        ERFnInterval 
            (UFB.setGranularity gran ln) (UFB.setGranularity gran h) 
            (ctxt { erfnCoeffGranularity = gran })
    setMinGranularityOuter gran (ERFnIntervalAny ctxt) = 
        ERFnIntervalAny
            (ctxt { erfnCoeffGranularity = max gran (erfnCoeffGranularity ctxt) })
    setMinGranularityOuter gran (ERFnInterval ln h ctxt) =
        ERFnInterval 
            (UFB.setMinGranularity gran ln) (UFB.setMinGranularity gran h) 
            (ctxt { erfnCoeffGranularity = max gran (erfnCoeffGranularity ctxt) })
    isBottom (ERFnIntervalAny c) = True
    isBottom _ = False
    bottomApprox = ERFnIntervalAny erfnContextDefault
--    isConsistent (ERFnIntervalAny _) = True 
--    isConsistent (ERFnInterval l r c) =
--        case RA.leqReals l r of
--            Just True -> True
--            _ -> False
    isBounded (ERFnIntervalAny c) = False
    isBounded _ = True
--    getPrecision (ERFnIntervalAny _) = 0
--    getPrecision f = intLog 2 (1 + (fst $ RA.integerBounds (FA.volume f))) -- wrong! 
    f1@(ERFnInterval ln1 h1 ctxt1) /\ f2@(ERFnInterval ln2 h2 ctxt2) =
---- #ifdef RUNTIME_CHECKS
----         check ("ERFnInterval: /\\:\n f1:\n" ++ show f1 ++ " f2:\n" ++ show f2 ++ "\n result:\n") $
---- #endif
        normalise $
        ERFnInterval 
            (UFB.minUp maxDegr maxSize ln1 ln2) 
            (UFB.minUp maxDegr maxSize h1 h2) 
            ctxt
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
        maxDegr = erfnMaxDegree ctxt
        maxSize = erfnMaxSize ctxt
    (ERFnIntervalAny ctxt1) /\ (ERFnInterval ln2 h2 ctxt2) =
        ERFnInterval ln2 h2 ctxt
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    (ERFnInterval ln1 h1 ctxt1) /\ (ERFnIntervalAny ctxt2) =
        ERFnInterval ln1 h1 ctxt
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    f1 /\ f2 = ERFnIntervalAny ctxt
        where
        ctxt = erfnContextUnify (erfnContext f1) (erfnContext f2)
    leqReals f1 f2 = 
--        unsafePrint ("ERInterval: leqReals: sizes: " ++ show (FA.getSize f1) ++ ", " ++ show (FA.getSize f2)) $ 
        erfnintLeq f1 f2
    refines _ (ERFnIntervalAny _) = True
    refines (ERFnIntervalAny _) _ = False
    refines (ERFnInterval ln1 h1 _) (ERFnInterval ln2 h2 _) = 
        (UFB.upperBound 10 (ln2 -^ ln1) >= 0)
        &&
        (UFB.upperBound 10 (h2 -^ h1) >= 0)
    compareApprox (ERFnIntervalAny _) (ERFnIntervalAny _) = EQ
    compareApprox (ERFnIntervalAny _) _ = LT
    compareApprox _ (ERFnIntervalAny _) = GT
    compareApprox (ERFnInterval ln1 h1 _) (ERFnInterval ln2 h2 _) =
        compareComposeMany
        [
            UFB.compareApprox h1 h2,
            UFB.compareApprox ln1 ln2
        ]

erfnintLeq left right
    | left `isClearlyBelow` right = Just True
    | right `isClearlyStrictlyBelow` left = Just False
    | otherwise = Nothing
    where
    isClearlyBelow (ERFnIntervalAny _) _ = False
    isClearlyBelow _ (ERFnIntervalAny _) = False
    isClearlyBelow f g
        | UFB.upperBound 10 (erfnUpper f +^ erfnLowerNeg g) <= 0 = True
        | otherwise = False
    isClearlyStrictlyBelow (ERFnIntervalAny _) _ = False
    isClearlyStrictlyBelow _ (ERFnIntervalAny _) = False
    isClearlyStrictlyBelow f g
        | UFB.upperBound 10 (erfnUpper f +^ erfnLowerNeg g) < 0 = True
        | otherwise = False

instance
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, Show varid, Show boxra, Show fb) =>
    RA.ERIntApprox (ERFnInterval fb) 
    where
--    doubleBounds = :: ira -> (Double, Double) 
--    floatBounds :: ira -> (Float, Float)
--    integerBounds :: ira -> (ExtendedInteger, ExtendedInteger)
    bisectDomain maybePt (ERFnIntervalAny c) =
        (ERFnIntervalAny c, ERFnIntervalAny c)
    bisectDomain maybePt (ERFnInterval ln h c) =
        (ERFnInterval ln midUp c,
         ERFnInterval midDownNeg h c)
         where
         (midDownNeg, midUp) =
            case maybePt of
                Nothing ->
                    (UFB.scaleUp (1/2) $ ln -^ h, UFB.scaleUp (1/2) $ h -^ ln)
                Just (ERFnInterval lnPt hPt _) ->
                    (lnPt, hPt)
    bounds (ERFnIntervalAny c) =
        error "ERFnInterval: RA.bounds: cannot get bounds for ERFnIntervalAny"
    bounds (ERFnInterval ln h c) =
        (ERFnInterval ln (UFB.neg ln) c,
         ERFnInterval (UFB.neg h) h c) 
    f1@(ERFnInterval ln1 h1 c1) \/ f2@(ERFnInterval ln2 h2 c2) =
---- #ifdef RUNTIME_CHECKS
----         check ("ERFnInterval: abs:\n f1:\n" ++ show f1 ++ " f2:\n" ++ show f2 ++ "\n result:\n") $
---- #endif
        normalise $
        ERFnInterval ln h c
        where
        ln = UFB.maxUp maxDegree maxSize ln1 ln2
        h = UFB.maxUp maxDegree maxSize h1 h2
        c = erfnContextUnify c1 c2
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
    (ERFnIntervalAny ctxt1) \/ (ERFnInterval ln2 h2 ctxt2) =
        ERFnIntervalAny ctxt
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    (ERFnInterval ln1 h1 ctxt1) \/ (ERFnIntervalAny ctxt2) =
        ERFnIntervalAny ctxt
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    f1 \/ f2 = ERFnIntervalAny ctxt
        where
        ctxt = erfnContextUnify (erfnContext f1) (erfnContext f2)

instance
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, 
     RAEL.ERApproxElementary ra, 
     Show varid, Show boxra, Show fb) =>
    RAEL.ERApproxElementary (ERFnInterval fb) 
    where
    -- default abs does not work because we do not have Prelude.abs
    abs _ f@(ERFnIntervalAny _) = f
    abs _ f@(ERFnInterval ln h c) =
---- #ifdef RUNTIME_CHECKS
----         check ("ERFnInterval: abs:\n f:\n" ++ show f ++ "\n result:\n") $
---- #endif
        normalise $
        ERFnInterval minhln0Up maxhlnUp c
        where
        maxhlnUp = UFB.maxUp maxDegree maxSize h ln 
        minhln0Up =
            UFB.minUp maxDegree maxSize (UFB.const 0) $
                UFB.minUp maxDegree maxSize h ln
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
    sqrt ix f@(ERFnIntervalAny _) = f
    sqrt ix f@(ERFnInterval ln h c)
        | nonNegative = 
            normalise $
            ERFnInterval lSqrtNeg hSqrt c
        | otherwise = 
            ERFnIntervalAny c -- could this be improved?
        where
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
        nonNegative =
            ((lnUpperBound < 0) || lnZero) 
            && 
            ((hLowerBound > 0) || hZero)
        lnUpperBound = UFB.upperBound ix ln
        lnLowerBound = UFB.lowerBound ix ln
        lnZero = (lnUpperBound == 0) && (lnLowerBound == 0)
        hLowerBound = UFB.lowerBound ix h  
        hUpperBound = UFB.upperBound ix h
        hZero = (hLowerBound == 0) && (hUpperBound == 0)
        -- compute for each bound separately, using monotonicity of sqrt:
        (lSqrtNeg, _) 
            | lnZero = (UFB.const 0, UFB.const 0) 
            | otherwise =
                UFB.sqrtEncl maxDegree maxSize ix (ln, UFB.neg ln)
        (_, hSqrt) 
            | hZero = (UFB.const 0, UFB.const 0)
            | otherwise =
                UFB.sqrtEncl maxDegree maxSize ix (UFB.neg h,h)
    exp ix f@(ERFnIntervalAny _) = f
    exp ix f@(ERFnInterval ln h c) = 
        normalise $
        ERFnInterval lExpNeg hExp c
        where
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
        (lExpNeg, hExp) =
--            case (UFB.upperBound ix (h +^ ln) <= 1) of
--                True -> 
--                    UFB.expEncl maxDegree maxSize ix (ln, h)
--                False ->
                    (lExpNeg, hExp)
                    where
                    -- compute for each bound separately, using monotonicity of exp:
                    (lExpNeg, _) = UFB.expEncl maxDegree maxSize ix (ln, UFB.neg ln)
                    (_, hExp) = UFB.expEncl maxDegree maxSize ix (UFB.neg h,h)
    sin ix f@(ERFnIntervalAny c) = 
        ERFnInterval one one c
        where
        one = UFB.const 1
    sin ix f@(ERFnInterval ln h c) =
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
        normalise $
        ERFnInterval lSinNeg hSin c
        where
        (lSinNeg, hSin) = sincos True maxDegree maxSize ix (ln, h)
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
    cos ix f@(ERFnIntervalAny c) =
        ERFnInterval one one c
        where
        one = UFB.const 1
    cos ix f@(ERFnInterval ln h c) =
--        unsafePrint
--        (
--            "ERFnInterval: RAEL.cos: "
--            ++ "\n h = " ++ show h
--            ++ "\n ln = " ++ show ln
--            ++ "\n uCos = " ++ show uCos
--            ++ "\n lCosNeg = " ++ show lCosNeg
--        ) $
        normalise $
        ERFnInterval lCosNeg hCos c
        where
        (lCosNeg, hCos) = sincos False maxDegree maxSize ix (ln,h) 
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
    atan ix f@(ERFnIntervalAny c) =
        ERFnInterval one one c
        where
        one = UFB.const 1
    atan ix f@(ERFnInterval ln h c) =
--        unsafePrint
--        (
--            "ERFnInterval: RAEL.atan: "
--            ++ "\n u = " ++ show u
--            ++ "\n ln = " ++ show ln
--            ++ "\n uAtan = " ++ show uAtan
--            ++ "\n lAtanNeg = " ++ show lAtanNeg
--        ) $
        normalise $
        ERFnInterval lAtanNeg hAtan c
        where
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
--        ix = int2effIx maxDegree
        (lAtanNeg, hAtan) = 
            case (UFB.upperBound ix (h +^ ln) <= 1) of
                True ->
                    UFB.atanEncl maxDegree maxSize ix (ln, h)
                False ->
                    (lAtanNeg, hAtan)
                    where
                    (lAtanNeg, _) = UFB.atanEncl maxDegree maxSize ix (ln, UFB.neg ln)
                    (_, hAtan) = UFB.atanEncl maxDegree maxSize ix (UFB.neg h,h)

sincos ::
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, RAEL.ERApproxElementary ra) =>
    Bool {-^ True iff sine, False iff cosine -} -> 
    Int {-^ maximum representation degree -} -> 
    Int {-^ maximum approx size -} -> 
    EffortIndex {-^ how hard to try to eliminate truncation errors -} -> 
    (fb, fb) ->
    (fb, fb)
sincos isSine maxDegreePP maxSize ix (ln,h)
    -- p - 2k*pi range within [-pi/2, pi/2]: 
    | ranfNear0 `RA.refines` plusMinusPiHalf =
--        unsafePrint
--        (
--            "ERFnInterval: sincos: [-pi/2, pi/2]: "
--            ++ "\n h = " ++ show h
--            ++ "\n ln = " ++ show ln
--            ++ "\n ranf = " ++ show ranf
--            ++ "\n k = " ++ show k
--            ++ "\n ranfNear0 = " ++ show ranfNear0
--        ) $
        mapPairHomog (UFB.reduceDegreeUp maxDegreePP) $
        case isSine of
            True -> sineShifted (- k2pi)
            False -> cosineShifted (- k2pi)
    -- p - 2k*pi range within [0, pi]: 
    | (ranfNear0 - piHalf) `RA.refines` plusMinusPiHalf =
--        unsafePrint
--        (
--            "ERFnInterval: sincos: [0, pi]: "
--            ++ "\n h = " ++ show h
--            ++ "\n ln = " ++ show ln
--            ++ "\n ranf = " ++ show ranf
--            ++ "\n k = " ++ show k
--            ++ "\n ranfNear0 = " ++ show ranfNear0
--            ++ "\n cosineShifted (- k2pi - piHalf) = " ++ show (cosineShifted (- k2pi - piHalf))
--        ) $
        mapPairHomog (UFB.reduceDegreeUp maxDegreePP) $
        case isSine of
            -- use sin(x) = cos(x - pi/2) and cos(x) = - sin(x - pi/2):
            True -> cosineShifted (- k2pi - piHalf)
            False -> sineShiftedNegated (- k2pi - piHalf)
    -- p - 2k*pi range within [-pi, 0]: 
    | (ranfNear0 + piHalf) `RA.refines` plusMinusPiHalf =
--        unsafePrint
--        (
--            "ERFnInterval: sincos: [-pi, 0]: "
--            ++ "\n h = " ++ show h
--            ++ "\n ln = " ++ show ln
--            ++ "\n ranf = " ++ show ranf
--            ++ "\n k = " ++ show k
--            ++ "\n ranfNear0 = " ++ show ranfNear0
--        ) $
        mapPairHomog (UFB.reduceDegreeUp maxDegreePP) $
        case isSine of
            -- use sin(x) = - cos(x + pi/2) and cos(x) = sin(x + pi/2):
            True -> cosineShiftedNegated (-k2pi + piHalf)
            False -> sineShifted (-k2pi + piHalf)
    -- p - 2k*pi range within [pi/2, 3pi/2]: 
    | (ranfNear0 - pi) `RA.refines` plusMinusPiHalf =
--        unsafePrint
--        (
--            "ERFnInterval: sincos: [pi/2, 3pi/2]: "
--            ++ "\n h = " ++ show h
--            ++ "\n ln = " ++ show ln
--            ++ "\n ranf = " ++ show ranf
--            ++ "\n k = " ++ show k
--            ++ "\n ranfNear0 = " ++ show ranfNear0
--        ) $
        -- use sin(x) = - sin(x - pi) and cos(x) = - cos(x - pi)
        mapPairHomog (UFB.reduceDegreeUp maxDegreePP) $
        case isSine of
            True -> sineShiftedNegated (- k2pi - pi)
            False -> cosineShiftedNegated (- k2pi - pi)
    | otherwise = 
--        unsafePrint
--        (
--            "ERFnInterval: sincos: big range: "
--            ++ "\n u = " ++ show u
--            ++ "\n l = " ++ show l
--            ++ "\n ranf = " ++ show ranf
--            ++ "\n k = " ++ show k
--            ++ "\n ranfNear0 = " ++ show ranfNear0
--        ) $
        (UFB.const (-1), UFB.const 1)
--    (expDownwards, expUpwards + valueAtRDnNeg + (UFB.const expRUp))
    where
    maxDegree = max 1 maxDegreePP
--    l = UFB.neg ln
    ranfNear0 = ranf - k2pi
    k2pi = k * 2 * pi
    plusMinusPiHalf = (-piHalfLO) RA.\/ piHalfLO
    pi = RAEL.pi ix  
    piHalf = pi / 2
    (piHalfLO, piHalfHI) = RA.bounds piHalf
    ranf = 
        ERInterval 
            (negate $ UFB.upperBound 10 ln) 
            (UFB.upperBound 10 h)
    k = fromInteger $ toInteger kEI
    (kEI,_) = RA.integerBounds $ 0.5 + (ranf / (2*pi))

    sineShiftedNegated shift =
        boundsNegate $ sineShifted shift
        
    cosineShiftedNegated shift =
        boundsNegate $ cosineShifted shift

    boundsNegate (pLONeg, pHI) = (pHI, pLONeg)
        
    sineShifted shift = -- moving to domain where sinus is non-decreasing
        case (UFB.upperBound ix (h +^ ln) <= 0.25) of
            True -> 
                UFB.sinEncl maxDegree maxSize ix (lnShifted, hShifted)
            False ->
                (lShiftedSinNeg, hShiftedSin)
                where
                (lShiftedSinNeg, _) = UFB.sinEncl maxDegree maxSize ix (lnShifted, UFB.neg lnShifted)
                (_, hShiftedSin) = UFB.sinEncl maxDegree maxSize ix (UFB.neg hShifted,hShifted)
        where
        lnShifted = ln +^ (UFB.const (- shiftLOB))
        hShifted = h +^ (UFB.const shiftHIB)
        ERInterval shiftLOB shiftHIB = shift


    
    cosineShifted shift = -- moving to domain where cosine is non-decreasing
        case (UFB.upperBound ix (h +^ ln) <= 0.25) of
            True -> 
--                unsafePrint
--                (
--                    "cosineShifted: thickness <= 0.25"
--                    ++ "\n ln = " ++ show ln
--                    ++ "\n h = " ++ show h
--                    ++ "\n shift = " ++ show shift
--                    ++ "\n lnShifted = " ++ show lnShifted
--                    ++ "\n hShifted = " ++ show hShifted
--                ) $
                UFB.cosEncl maxDegree maxSize ix (lnShifted, hShifted)
            False ->
--                unsafePrint
--                (
--                    "cosineShifted: thick version"
--                    ++ "\n ln = " ++ show ln
--                    ++ "\n h = " ++ show h
--                    ++ "\n shift = " ++ show shift
--                    ++ "\n lnShifted = " ++ show lnShifted
--                    ++ "\n hShifted = " ++ show hShifted
--                ) $
                (UFB.maxUp maxDegree maxSize lShiftedCosDownNeg hShiftedCosDownNeg,
                 UFB.maxUp maxDegree maxSize lShiftedCosUp hShiftedCosUp 
                    +^ (UFB.scaleUp 0.5 (hShiftedCosUp +^ lnShifted))) 
                        -- this term is important when enclosure hits 0;
                        -- without it, the result could miss cosine's maximum at 0
        where
        (lShiftedCosDownNeg, lShiftedCosUp) = 
            UFB.cosEncl maxDegree maxSize ix (lnShifted, UFB.neg lnShifted)
        (hShiftedCosDownNeg, hShiftedCosUp) = 
            UFB.cosEncl maxDegree maxSize ix (UFB.neg hShifted,hShifted)
        lnShifted = ln +^ (UFB.const (- shiftLOB))
        hShifted = h +^ (UFB.const shiftHIB)
        ERInterval shiftLOB shiftHIB = shift
    
    boundsAddErr errB (pLONeg, pHI) =
        (pLONeg +^ errPoly, pHI +^ errPoly)
        where
        errPoly = UFB.const errB

normalise f@(ERFnIntervalAny c) = f
normalise f@(ERFnInterval ln h c)
    | UFB.isValid h && UFB.isValid ln = f
    | otherwise = ERFnIntervalAny c 
    
check callerLocation f@(ERFnIntervalAny c) = f
check callerLocation f@(ERFnInterval ln h c) =
    ERFnInterval 
        (UFB.check (callerLocation ++ "upper: ") h) 
        (UFB.check (callerLocation ++ "neg lower: ") ln) 
        c


instance 
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, Show varid, Show boxra, Show fb) =>
    FA.ERFnApprox boxra varid ra ra (ERFnInterval fb)
    where
    check = check
    domra2ranra _ = id
    ranra2domra _ = id
    getMaxDegree (ERFnIntervalAny c) =
        erfnMaxDegree c
    getMaxDegree (ERFnInterval _ _ c) =
        erfnMaxDegree c
    setMaxDegree maxDegr (ERFnIntervalAny c) =
        ERFnIntervalAny (c { erfnMaxDegree = maxDegr } )
    setMaxDegree maxDegr (ERFnInterval ln h c) =
        ERFnInterval 
            (UFB.reduceDegreeUp maxDegr ln)
            (UFB.reduceDegreeUp maxDegr h)
            (c { erfnMaxDegree = maxDegr } )
    getSize (ERFnIntervalAny c) = 0
    getSize (ERFnInterval ln h c) =
        max (UFB.getSize ln) (UFB.getSize h)
    getMaxSize (ERFnIntervalAny c) =
        erfnMaxSize c
    getMaxSize (ERFnInterval _ _ c) =
        erfnMaxSize c
    setMaxSize maxSize (ERFnIntervalAny c) =
        ERFnIntervalAny (c { erfnMaxDegree = maxSize } )
    setMaxSize maxSize (ERFnInterval ln h c) =
        ERFnInterval 
            (UFB.reduceSizeUp maxSize ln)
            (UFB.reduceSizeUp maxSize h)
            (c { erfnMaxSize = maxSize } )
    getVariables (ERFnIntervalAny _) = []
    getVariables (ERFnInterval ln h _) = UFB.getVariables h 
    getRangeApprox (ERFnIntervalAny _) = 
        [RA.bottomApprox] 
    getRangeApprox (ERFnInterval ln h c) =
        [UFB.raFromEndpoints h
        (
         (- (UFB.upperBound 10 ln))
        ,
         (UFB.upperBound 10 h)
        )]
    scale ratio f@(ERFnIntervalAny c) = 
        f
    scale ratio f@(ERFnInterval ln h c) =
---- #ifdef RUNTIME_CHECKS
----         FA.check ("ERFnInterval: scale:\n before:\n" ++ show f ++ "\n after:\n") $
---- #endif
        normalise $
        case RA.compareReals ratio 0 of
            Just GT -> 
                ERFnInterval (scaleUp ratio ln) (scaleUp ratio h) c
            Just LT -> 
                ERFnInterval (scaleUp (- ratio) h) (scaleUp (- ratio) ln) c
            _ -> 
                (UFA.const [ratio]) * f
        where
        scaleUp = UFB.scaleApproxUp maxDegree maxSize
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
    eval ptBox (ERFnIntervalAny c) = [RA.bottomApprox]
    eval ptBox (ERFnInterval ln h c) =
        [RA.fromBounds (lo,up)]
        where
        (_, up) = RA.bounds $ UFB.evalApprox ptBox h
        (lo, _) = RA.bounds $ negate $ UFB.evalApprox ptBox ln
    evalInner ptBox (ERFnIntervalAny c) = [RA.bottomApprox]
    evalInner ptBox (ERFnInterval ln h c) =
        [RA.fromBounds (lo,up)]
        where
        (up, _) = RA.bounds $ UFB.evalApprox ptBox h
        (_, lo) = RA.bounds $ negate $ UFB.evalApprox ptBox ln
    partialEval substitutions f@(ERFnIntervalAny c) = f
    partialEval substitutions f@(ERFnInterval ln h c) =
        normalise $
        ERFnInterval lnP hP c
        where
        hP = UFB.partialEvalApproxUp substitutions h
        lnP = UFB.partialEvalApproxUp substitutions ln
    compose
            fOuter@(ERFnInterval lnOuter hOuter cOuter)
            varid
            fInner@(ERFnInterval lnInner hInner cInner) =
        result
        where
        result = ERFnInterval ln h c
        (_,h) = UFB.composeEncl maxDegree maxSize hOuter varid (lnInner, hInner)
        (_,ln) = UFB.composeEncl maxDegree maxSize lnOuter varid (lnInner, hInner)
        c = erfnContextUnify cOuter cInner
        maxDegree = erfnMaxDegree c        
        maxSize = erfnMaxSize c        
    composeNonDecreasing
            fOuter@(ERFnInterval lnOuter hOuter cOuter)
            varid
            fInner@(ERFnInterval lnInner hInner cInner) =
--        unsafePrintReturn
--        (
--            "ER.RnToRm.UnitDom.Interval: composeNonDecreasing: "
--            ++ "\n fOuter = " ++ show fOuter
--            ++ "\n varid = " ++ show varid
--            ++ "\n fInner = " ++ show fInner
----            ++ "\n inconsistencies = " ++ show (UFA.keyPointsConsistencyCheck resultReals result)
--            ++ "\n result = "
--        )
--        $
        result
        where
        result = ERFnInterval ln h c
        h =
            erfnUpper $ 
                UFA.composeWithThin fOuter $
                    Map.singleton varid
                    (ERFnInterval (UFB.neg hInner) hInner cInner)
        ln =
            erfnLowerNeg $
                UFA.composeWithThin fOuter $
                    Map.singleton varid $
                    (ERFnInterval lnInner (UFB.neg lnInner) cInner)
        c = erfnContextUnify cOuter cInner
        
    composeNonDecreasing fOuter varid fInner = 
        ERFnIntervalAny c
        where
        c = erfnContextUnify (erfnContext fOuter) (erfnContext fInner)

instance 
    (UFB.ERUnitFnBaseElementary boxb boxra varid b ra fb, Show varid, Show boxra, Show fb) =>
    UFA.ERUnitFnApprox boxra varid ra ra (ERFnInterval fb)
    where
    bottomApprox =
        ERFnIntervalAny erfnContextDefault
    const [val] 
        | RA.isBounded val =
---- #ifdef RUNTIME_CHECKS
----             check ("ERFnInterval: const:\n") $
---- #endif
            normalise $
            ERFnInterval
            {
                erfnLowerNeg = fbLNeg,
                erfnUpper = fbH,
                erfnContext = context
            }
        | otherwise =
            ERFnIntervalAny context 
        where
        fbH = UFB.const valH
        fbLNeg = UFB.const (negate valL)
        (valL, valH) = UFB.raEndpoints fbH val
        context = 
            erfnContextDefault
            {
                erfnMaxDegree = 0,
                erfnMaxSize = 1,
                erfnCoeffGranularity = RA.getGranularity val
            }
    affine [val] coeffsSingletons
        | RA.isBounded val && (and $ map (RA.isBounded . head) $ Map.elems coeffsSingletons) =
---- #ifdef RUNTIME_CHECKS
----             check ("ERFnInterval: affine:\n") $
---- #endif
            normalise $
            ERFnInterval
            {
                erfnLowerNeg = fbLNeg,
                erfnUpper = fbH,
                erfnContext = context
--                ,
--                erfnGlobal = 
--                    UFB.raFromEndpoints fbH
--                        (valL - coeffCorr - coeffsAbsSum, 
--                         valH + coeffCorr + coeffsAbsSum)
            }
        | otherwise =
            ERFnIntervalAny context
        where
        coeffs = Map.map (\[a] -> a) coeffsSingletons
        coeffGranularity =
            Map.fold max (RA.getGranularity val) (Map.map RA.getGranularity coeffs)
        coeffsMsCorrs = 
            Map.map (\(l,h) ->
                    (B.setMinGranularity coeffGranularity (l + h)/2, 
                     B.setMinGranularity coeffGranularity (h - l)/2)) $
            Map.map (UFB.raEndpoints fbH) $ coeffs
        coeffCorr = Map.fold (+) 0 $ Map.map snd coeffsMsCorrs
        coeffsAbsSum = Map.fold (+) 0 $ Map.map (abs . fst) coeffsMsCorrs
        fbH = UFB.affine (valH + coeffCorr)  (Map.map fst coeffsMsCorrs)
        fbLNeg = UFB.affine (negate (valL - coeffCorr)) (Map.map (negate . fst) coeffsMsCorrs)
        (valL, valH) = UFB.raEndpoints fbH val
        context = 
            erfnContextDefault
            {
                erfnCoeffGranularity = coeffGranularity
            }
    composeWithThin
            f@(ERFnIntervalAny ctxt)
            substitutions =
        f
    composeWithThin
            f@(ERFnInterval ln1 h1 ctxt1)
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
                
        result = ERFnInterval ln h ctxt1
        ln = UFB.composeManyUp maxDegree maxSize ln1 ufbSubstitutions
        h = UFB.composeManyUp maxDegree maxSize h1 ufbSubstitutions 
        ufbSubstitutions = Map.map erfnUpper substitutions
        maxDegree = erfnMaxDegree ctxt1        
        maxSize = erfnMaxSize ctxt1        
--        ctxt = erfnContextUnify ctxt1 ctxt2
    intersectMeasureImprovement ix vars
            f1@(ERFnIntervalAny ctxt1) 
            f2@(ERFnIntervalAny ctxt2) =
        (ERFnIntervalAny ctxt, RA.bottomApprox)
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    intersectMeasureImprovement ix vars
            f1@(ERFnIntervalAny ctxt1) 
            f2@(ERFnInterval ln2 h2 ctxt2) =
        (ERFnInterval ln2 h2 ctxt, RA.plusInfinity)
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    intersectMeasureImprovement ix vars
            f1@(ERFnInterval ln1 h1 ctxt1) 
            f2@(ERFnIntervalAny ctxt2) = 
        (ERFnInterval ln1 h1 ctxt, 1)
        where
        ctxt = erfnContextUnify ctxt1 ctxt2
    intersectMeasureImprovement ix vars
            f1@(ERFnInterval ln1 h1 ctxt1) 
            f2@(ERFnInterval ln2 h2 ctxt2) =
        case RA.compareReals improvementRA 1 of
            Just LT -> (f1, 1) -- intersection made it worse, keep original
            _ ->  (intersection, improvementRA)
        where
        intersection = 
---- #ifdef RUNTIME_CHECKS
----             check ("ERFnInterval: intersectMeasureImprovement:\n f1:\n" ++ show f1 ++ "\n f2:\n" ++ show f2 ++ "\n intersection:\n") $
---- #endif
            normalise $
            f1 RA./\ f2
        improvementRA 
            | 0 `RA.refines` intersectionVolume && 0 `RA.refines` f1Volume = 1
--                error $ 
--                    "ERFnInterval: intersectMeasureImprovement: inconsistent result: " 
--                    ++ show intersection
            | otherwise =
                 f1Volume / intersectionVolume
        intersectionVolume = UFA.volume vars intersection
        f1Volume = UFA.volume vars f1
        ctxt = erfnContextUnify ctxt1 ctxt2
    volume vars (ERFnIntervalAny c) = RA.plusInfinity
    volume vars (ERFnInterval ln h c) =
        UFB.raFromEndpoints h (volL, volH)
        where 
        volH = UFB.volumeAboveZeroUp vars (ln +^ h)
        volL = negate $ UFB.volumeAboveZeroUp vars (l +^ hn)
        l = UFB.neg ln
        hn = UFB.neg h
    integrate _ f@(ERFnIntervalAny c) _ _ _ = f 
    integrate 
            ix fD@(ERFnInterval ln h c) x 
            origin fI@(ERFnInterval lnInit hInit cInit) =
--        unsafePrintReturn
--        (
--            "ERFnInterval: integrate: " 
--            ++ "\n maxDegree = " ++ show maxDegree
--            ++ "\n maxSize = " ++ show maxSize
--            ++ "\n h = " ++ show h
--            ++ "\n ln = " ++ show ln
--            ++ "\n origin = " ++ show origin
--            ++ "\n hInit = " ++ show hInit
--            ++ "\n lnInit = " ++ show lnInit
--            ++ "\n hIuL = " ++ show hIuL
--            ++ "\n hIuH = " ++ show hIuH
--            ++ "\n hIuOriginLNeg = " ++ show hIuOriginLNeg
--            ++ "\n hIuOriginH = " ++ show hIuOriginH
--            ++ "\n lnIuL = " ++ show lnIuL
--            ++ "\n lnIuH = " ++ show lnIuH
--            ++ "\n lnIuOriginLNeg = " ++ show lnIuOriginLNeg
--            ++ "\n lnIuOriginU = " ++ show lnIuOriginH
--            ++ "\n hIov = " ++ show hIov
--            ++ "\n lnIov = " ++ show lnIov
--            ++ "\n result = "
--        )
--        $
---- #ifdef RUNTIME_CHECKS
----         check ("ERFnInterval: integrate:\n fD:\n" ++ show fD ++ "\n fI:\n" ++ show fI ++ "\n result:\n") $
---- #endif
        normalise $
        (ERFnInterval lnIov hIov cPlus1)
        where
        cPlus1 = c { erfnMaxDegree = (erfnMaxDegree c) + 1}
        -- perform raw integration of both bounds:
        (hIuL, hIuH) = 
--            mapPair (UFB.reduceDegreeDown maxDegree, UFB.reduceDegreeUp maxDegree) $ 
                UFB.integrate x h
        (lnIuL, lnIuH) = 
--            mapPair (UFB.reduceDegreeDown maxDegree, UFB.reduceDegreeUp maxDegree) $ 
                UFB.integrate x ln
        maxDegree = erfnMaxDegree c
        maxSize = erfnMaxSize c
        -- constrain the raw integrals to the origin:
        (hIuOriginLNeg, hIuOriginH) =
            UFB.composeEncl maxDegree maxSize hIuL x originEncl
        (lnIuOriginLNeg, lnIuOriginH) = 
            UFB.composeEncl maxDegree maxSize lnIuL x originEncl
        originEncl = UFB.constEncl $ UFB.raEndpoints h origin
        -- adjust the raw integrated functions to enclose the initial condition function:                        
        hIov = 
            UFB.reduceSizeUp maxSize $
                hIuH +^ hInit +^ hIuOriginLNeg +^ (hIuOriginH +^ hIuOriginLNeg)
        lnIov = 
            UFB.reduceSizeUp maxSize $
                lnIuH +^ lnInit +^ lnIuOriginLNeg +^ (lnIuOriginH +^ lnIuOriginLNeg)

