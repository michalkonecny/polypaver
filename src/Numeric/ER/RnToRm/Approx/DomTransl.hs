{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
    Module      :  Numeric.ER.RnToRm.Approx.DomTransl
    Description :  enclosures translated from [-1,1]^n to another domain 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Datatype translating enclosures from @[-1,1]^n@ to any compact
    interval in @R^n@ with non-empty interior.
-}
-- #define ASSUME_DOMAINS_COMPATIBLE
module Numeric.ER.RnToRm.Approx.DomTransl 
(
    ERFnDomTranslApprox(..), DomTransl(..)
)
where
import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.RnToRm.UnitDom.Approx as UFA
import qualified Numeric.ER.Real.Approx as RA
import Numeric.ER.Real.Approx ((+:),(-:),(*:),(/:))
import qualified Numeric.ER.Real.Approx.Elementary as RAEL
import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainIntBox, DomainBoxMappable)
import Numeric.ER.BasicTypes
import Numeric.ER.Misc

import Numeric.ER.RnToRm.UnitDom.Approx.Interval

import qualified Text.Html as H

import Data.Typeable
import Data.Generics.Basics
import Data.Binary

import qualified Data.Map as Map

{-|
    Datatype translating enclosures from @[-1,1]^n@ to any compact
    interval in @R^n@ with non-empty interior
    using a bunch of linear maps, one for each dimension. 
-}
data ERFnDomTranslApprox dtrbox varid ufa ira =
    ERFnDomTranslApprox
    {
        erfnUnitApprox :: ufa,
        erfnDomTransl :: dtrbox
    }
    deriving (Typeable, Data)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (ERFnDomTranslApprox a b c d) where
  put (ERFnDomTranslApprox a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (ERFnDomTranslApprox a b)
    
{-| 
    The canonical translation of 
    any compact non-empty and non-singleton interval in @R@
    to and from the unit interval @[-1,1]@.
    
    This structure holds the two coefficients for both
    linear mappings.
-}
data DomTransl ira =
    DomTransl
    {
        dtrDom :: ira {-^ the interval being mapped -},
        dtrFromUnitSlope :: ira,
        dtrFromUnitConst :: ira,
        dtrToUnitSlope :: ira,
        dtrToUnitConst :: ira
    }
    deriving (Typeable, Data)
    
instance (Binary a) => Binary (DomTransl a) where
  put (DomTransl a b c d e) = put a >> put b >> put c >> put d >> put e
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (DomTransl a b c d e)
    
instance
    (RA.ERIntApprox domra) =>
    Eq (DomTransl domra)
    where
    (DomTransl _ _ _ _ dom1) == (DomTransl _ _ _ _ dom2) =
        RA.equalApprox dom1 dom2
    
instance
    (RA.ERIntApprox domra) =>
    Ord (DomTransl domra)
    where
    compare (DomTransl _ _ _ _ dom1) (DomTransl _ _ _ _ dom2) =
        RA.compareApprox dom1 dom2
    
instance
    (RA.ERIntApprox domra) =>
    Show (DomTransl domra)
    where
    show (DomTransl fromA fromB toA toB dom) =
        "DomTransl\n" ++   
        "  dom = " ++ show dom ++ "\n" ++
        "  fromUnit = " ++ show fromA ++ " * x + " ++ show fromB ++ "\n" ++
        "  toUnit = " ++ show toA ++ " * x + " ++ show toB ++ "\n"

instance
    (RA.ERIntApprox domra, H.HTML domra) =>
    H.HTML (DomTransl domra)
    where
    toHtml (DomTransl fromA fromB toA toB dom) =
        error "DomTransl: toHtml not implemented yet"



dtrIdentity ::
    (RA.ERIntApprox ira) =>
    DomTransl ira
dtrIdentity =
    makeDomTransl ((-1) RA.\/ 1)
    
dtrBToDomB dtrB =
    DBox.map dtrDom dtrB
    
makeDomTransl ::
    (RA.ERIntApprox ira) =>
    ira ->
    DomTransl ira
makeDomTransl dom 
    | domSuitable =
        DomTransl
        {
            dtrFromUnitSlope = dHMdL / 2,
            dtrFromUnitConst = dHPdL / 2,
            dtrToUnitSlope = 2 / dHMdLgr,
            dtrToUnitConst = - dHPdL / dHMdLgr,
            dtrDom = dom
        }
    | otherwise =
        error $ 
            "DomTranslApprox: makeDomTransl: cannot make a translation to domain " 
            ++ show dom
    where
    domSuitable = RA.isBounded dom && (not $ RA.isExact dom)
    (dL, dH) = RA.bounds dom
    dHPdL = dH + dL
    dHMdL = dH - dL
    dHMdLgr = RA.setMinGranularityOuter 100 dHMdL
--    fromUnit x = (x * (dHMdL) + dHPdL) / 2 
--    toUnit y = (2 * y - dHPdL) / dHMdL

dtrToUnit domTransl x = a * x + b
    where
    a = dtrToUnitSlope domTransl
    b = dtrToUnitConst domTransl
    
dtrToUnitInner domTransl x = a *: x +: b
    where
    a = dtrToUnitSlope domTransl
    b = dtrToUnitConst domTransl
    
dtrFromUnit domTransl x = a * x + b
    where
    a = dtrFromUnitSlope domTransl
    b = dtrFromUnitConst domTransl

domToUnit ::
--    (DomainIntBox dbox varid ira, 
--     DomainIntBox dtrbox varid (DomTransl ira)) =>
    (DomainBoxMappable dbox dtrbox varid ira (DomTransl ira),
     Num ira) => 
    dtrbox -> dbox -> dbox
domToUnit dtrB domBox =
    DBox.intersectionWith (\d dtr -> dtrToUnit dtr d) domBox dtrB

domToUnitInner ::
--    (DomainIntBox dbox varid ira, 
--     DomainIntBox dtrbox varid (DomTransl ira)) =>
    (DomainBoxMappable dbox dtrbox varid ira (DomTransl ira),
     RA.ERApprox ira, RA.ERInnerOuterApprox ira) => 
    dtrbox -> dbox -> dbox
domToUnitInner dtrB domBox =
    DBox.intersectionWith (\d dtr -> dtrToUnitInner dtr d) domBox dtrB

#ifdef ASSUME_DOMAINS_COMPATIBLE

dtrsCompatible _ _ = True

dtrUnion msg dtr1 dtr2 = dtr1

#else    
dtrsCompatible dtr1 dtr2 =
    foldl (&&) True $ map snd $ 
        DBox.zipWith eqDomains dtr1 dtr2
    where
    eqDomains d1 d2 =
        d1L == d2L && d1U == d2U
        where
        (d1L, d1U) = RA.bounds $ dtrDom d1
        (d2L, d2U) = RA.bounds $ dtrDom d2

dtrUnion msg dtr1 dtr2 
    | dtrsCompatible dtr1 dtr2 = 
        DBox.union dtr1 dtr2
    | otherwise = error msg

#endif

dtrBShow dtrB =
    concatWith "," $
    map showOne $ DBox.toList dtrB
    where
    showOne (var, dtr) =
        showVar var ++ " in " ++ show (dtrDom dtr)
        
    
instance 
    (UFA.ERUnitFnApprox box varid domra ranra ufa, 
     DomainBoxMappable dtrbox box varid (DomTransl domra) domra) =>
    Show (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    show (ERFnDomTranslApprox ufa dtrB) =
        "\nERFnDomTranslApprox" ++
        show ufaDom ++
--        show ufa ++
        "\n dom = [" ++ dtrBShow dtrB ++ "]"
        where
        ufaDom =
            translateUfaToDom ufa dtrB
--        gr = 20 + (RA.getGranularity ufa)

translateUfaToDom ufa dtrB = -- this is unsafe, use only for printing!
    UFA.composeWithThin ufa $  
        Map.fromAscList $ 
            map mkToUnitUFA $ 
                 DBox.toAscList dtrB
    where
    mkToUnitUFA (var, tr) =
        (var, UFA.affine [co] (Map.singleton var [sl]))
        where
        sl = FA.domra2ranra ufa $ dtrToUnitSlope tr
        co = FA.domra2ranra ufa $ dtrToUnitConst tr


instance 
    (UFA.ERUnitFnApprox box varid domra ranra ufa,
     DomainBoxMappable dtrbox box varid (DomTransl domra) domra, 
     DomainBoxMappable box dtrbox varid domra (DomTransl domra),
     H.HTML ufa) =>
    H.HTML (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    toHtml (ERFnDomTranslApprox ufa dtrB) =
        H.toHtml $ translateUfaToDom ufa dtrB
--        H.toHtml $
--            abovesTable []
--                [
--                    H.toHtml $ "enclosure on dom = [" ++ dtrBShow dtrB ++ "]",
--                    H.toHtml $ translateUfaToDom ufa dtrB
--                ]

instance 
    (UFA.ERUnitFnApprox box varid domra ranra ufa, 
     Eq dtrbox) =>
    Eq (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    (ERFnDomTranslApprox ufa1 dtrB1) == (ERFnDomTranslApprox ufa2 dtrB2) =
        ufa1 == ufa2 && dtrB1 == dtrB2

instance 
    (UFA.ERUnitFnApprox box varid domra ranra ufa, Ord ufa
    , Eq dtrbox) =>
    Ord (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    compare (ERFnDomTranslApprox ufa1 dtrB1) (ERFnDomTranslApprox ufa2 dtrB2)
        | dtrB1 == dtrB2 =
                compare ufa1 ufa2
        | otherwise =
            error "DomTransl: compare: incompatible domains"  

instance
    (UFA.ERUnitFnApprox box varid domra ranra ufa, 
     DomainBoxMappable dtrbox box varid (DomTransl domra) domra, Eq dtrbox) =>
    Num (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    fromInteger n = ERFnDomTranslApprox (fromInteger n) DBox.noinfo
    negate (ERFnDomTranslApprox ufa dtrB) =
        (ERFnDomTranslApprox (negate ufa) dtrB)
    (ERFnDomTranslApprox ufa1 dtr1) + (ERFnDomTranslApprox ufa2 dtr2) =
        ERFnDomTranslApprox (ufa1 + ufa2) (dtrUnion msg dtr1 dtr2)
        where
        msg = "DomTransl: cannot add approximations with incompatible domains"
    (ERFnDomTranslApprox ufa1 dtr1) * (ERFnDomTranslApprox ufa2 dtr2) = 
        ERFnDomTranslApprox (ufa1 * ufa2) (dtrUnion msg dtr1 dtr2)
        where
        msg = "DomTransl: cannot multiply approximations with incompatible domains"
        
instance 
    (UFA.ERUnitFnApprox box varid domra ranra ufa
    , DomainBoxMappable dtrbox box varid (DomTransl domra) domra, Eq dtrbox) =>
    Fractional (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    fromRational r = ERFnDomTranslApprox (fromRational r) DBox.noinfo
    recip f@(ERFnDomTranslApprox ufa dtrB) =
--        unsafePrintReturn ("DomTransl: recip of " ++ show f ++ "\n = ") $
        ERFnDomTranslApprox (recip ufa) dtrB

instance 
    (UFA.ERUnitFnApprox box varid domra ranra ufa
    , DomainBoxMappable dtrbox box varid (DomTransl domra) domra, Eq dtrbox, Ord dtrbox) =>
    RA.ERApprox (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    initialiseBaseArithmetic _ =
    	RA.initialiseBaseArithmetic (0 :: ufa)
    getGranularity (ERFnDomTranslApprox ufa dtrB) =
        RA.getGranularity ufa
    setGranularityOuter gran (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (RA.setGranularityOuter gran ufa) dtrB
    setMinGranularityOuter gran (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (RA.setMinGranularityOuter gran ufa) dtrB
    isBottom = RA.isBottom . erfnUnitApprox
    isBounded = RA.isBounded . erfnUnitApprox
    (ERFnDomTranslApprox ufa1 dtrB1) /\ (ERFnDomTranslApprox ufa2 dtrB2) =
        ERFnDomTranslApprox (ufa1 RA./\ ufa2) (dtrUnion msg dtrB1 dtrB2)
        where
        msg = "DomTransl: cannot intersect approximations with incompatible domains"
    intersectMeasureImprovement ix 
            (ERFnDomTranslApprox ufa1 dtrB1) 
            (ERFnDomTranslApprox ufa2 dtrB2) = 
        (ERFnDomTranslApprox ufaIsect dtrB,
         ERFnDomTranslApprox ufaImpr dtrB)
        where
        (ufaIsect, raImpr) = UFA.intersectMeasureImprovement ix vars ufa1 ufa2
        ufaImpr = UFA.const [raImpr]
        dtrB = dtrUnion msg dtrB1 dtrB2
        msg = "DomTransl: cannot intersect approximations with incompatible domains"
        vars = DBox.keys dtrB
    leqReals fa1 fa2 =
        RA.leqReals (erfnUnitApprox fa1) (erfnUnitApprox fa2)
    refines fa1 fa2 =
        RA.refines (erfnUnitApprox fa1) (erfnUnitApprox fa2)
    compareApprox (ERFnDomTranslApprox ufa1 dtrB1) (ERFnDomTranslApprox ufa2 dtrB2) =
        compareComposeMany
            [ 
                RA.compareApprox ufa1 ufa2,
                compare dtrB1 dtrB2
            ]

instance (RA.ERApproxApprox ufa) => 
    RA.ERApproxApprox (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    safeIncludes fa1 fa2 =
        RA.safeIncludes (erfnUnitApprox fa1) (erfnUnitApprox fa2)
    safeNotIncludes fa1 fa2 =
        RA.safeNotIncludes (erfnUnitApprox fa1) (erfnUnitApprox fa2)
    includes fa1 fa2 =
        RA.includes (erfnUnitApprox fa1) (erfnUnitApprox fa2)

instance
    (UFA.ERUnitFnApprox box varid domra ranra ufa, RA.ERIntApprox ufa 
    , DomainBoxMappable dtrbox box varid (DomTransl domra) domra, Eq dtrbox, Ord dtrbox) =>
    RA.ERIntApprox (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    doubleBounds (ERFnDomTranslApprox ufa _) = RA.doubleBounds ufa 
--    floatBounds :: ira -> (Float, Float)
--    integerBounds :: ira -> (ExtendedInteger, ExtendedInteger)
    bisectDomain maybePt (ERFnDomTranslApprox ufa dtrB) =
        (ERFnDomTranslApprox ufa1 dtrB,
         ERFnDomTranslApprox ufa2 dtrB)
         where
         (ufa1, ufa2) = RA.bisectDomain (fmap erfnUnitApprox maybePt) ufa
    bounds (ERFnDomTranslApprox ufa dtrB) =
        (ERFnDomTranslApprox ufa1 dtrB,
         ERFnDomTranslApprox ufa2 dtrB)
         where
         (ufa1, ufa2) = RA.bounds ufa
    (ERFnDomTranslApprox ufa1 dtrB1) \/ (ERFnDomTranslApprox ufa2 dtrB2) =
        ERFnDomTranslApprox (ufa1 RA.\/ ufa2) (dtrUnion msg dtrB1 dtrB2)
        where
        msg = "DomTransl: cannot intersect approximations with incompatible domains"

instance
    (UFA.ERUnitFnApprox box varid domra ranra ufa, RAEL.ERApproxElementary ufa
    , DomainBoxMappable dtrbox box varid (DomTransl domra) domra, Eq dtrbox, Ord dtrbox) =>
    RAEL.ERApproxElementary (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    abs ix (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (RAEL.abs ix ufa) dtrB
    sqrt ix f@(ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (RAEL.sqrt ix ufa) dtrB
    exp ix f@(ERFnDomTranslApprox ufa dtrB) =
--        unsafePrintReturn ("DomTransl: exp of " ++ show f ++ "\n = ") $
        ERFnDomTranslApprox (RAEL.exp ix ufa) dtrB
    log ix (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (RAEL.log ix ufa) dtrB
    sin ix (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (RAEL.sin ix ufa) dtrB
    cos ix (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (RAEL.cos ix ufa) dtrB
    atan ix (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (RAEL.atan ix ufa) dtrB

instance 
    (UFA.ERUnitFnApprox box varid domra ranra ufa,
     RA.ERInnerOuterApprox domra, 
     DomainBoxMappable dtrbox box varid (DomTransl domra) domra, 
     DomainIntBox box varid domra, 
     Show varid, Show box,
     DomainBoxMappable box dtrbox varid domra (DomTransl domra), 
     Eq dtrbox, Ord dtrbox) =>
    FA.ERFnApprox box varid domra ranra (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    check prgLocation (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (FA.check (prgLocation ++ dtrBShow dtrB ++ ": ") ufa) dtrB
    domra2ranra fa d =
        FA.domra2ranra (erfnUnitApprox fa) d
    ranra2domra fa r =
        FA.ranra2domra (erfnUnitApprox fa) r
    getMaxDegree (ERFnDomTranslApprox ufa _) =
        FA.getMaxDegree ufa
    setMaxDegree maxDegree (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (FA.setMaxDegree maxDegree ufa) dtrB
    getMaxSize (ERFnDomTranslApprox ufa _) =
        FA.getMaxSize ufa
    setMaxSize maxSize (ERFnDomTranslApprox ufa dtrB) =
        ERFnDomTranslApprox (FA.setMaxSize maxSize ufa) dtrB
    getRangeApprox (ERFnDomTranslApprox ufa dtrB) =
        FA.getRangeApprox ufa
    volume (ERFnDomTranslApprox ufa dtrB) =
        DBox.fold 
            (\tr vol -> vol * (FA.domra2ranra ufa $ dtrFromUnitSlope tr)) 
            (UFA.volume vars ufa) dtrB 
        where
        vars = DBox.keys dtrB
    scale ratio (ERFnDomTranslApprox ufa dtrB) =
        (ERFnDomTranslApprox (FA.scale ratio ufa) dtrB)
    partialIntersect ix substitutions f1 f2 
        | insideSubstitutions = f1 RA./\ f2
        | otherwise = f2
        where
        insideSubstitutions =
            and $ map snd $
                DBox.zipWith (RA.refines) dom1 substitutions
        dom1 = FA.dom f2
    eval ptBox (ERFnDomTranslApprox ufa dtrB) =
        FA.eval (domToUnit dtrB ptBox) ufa
    evalInner ptBox (ERFnDomTranslApprox ufa dtrB) =
        FA.evalInner (domToUnitInner dtrB ptBox) ufa
    partialEval substitutions (ERFnDomTranslApprox ufa dtrB) =
        (ERFnDomTranslApprox (FA.partialEval (domToUnit dtrB substitutions) ufa) dtrBNoVars)
        where
        dtrBNoVars =
            DBox.difference dtrB substitutions
    composeNonDecreasing
        fOuter@(ERFnDomTranslApprox ufaOuter dtrBOuter)
        varid
        fInner@(ERFnDomTranslApprox ufaInner dtrBInner)
        =
--        unsafePrintReturn
--        (
--            "ER.RnToRm.DomTransl: composeNonDecreasing: "
--            ++ "\n fOuter = " ++ show fOuter
--            ++ "\n ufaOuter = " ++ show ufaOuter
--            ++ "\n dtrVarConst = " ++ show dtrVarConst
--            ++ "\n dtrVarSlope = " ++ show dtrVarSlope
--            ++ "\n varid = " ++ show varid
--            ++ "\n fInner = " ++ show fInner
--            ++ "\n ufaInner = " ++ show ufaInner
--            ++ "\n ufaInnerUnitDom = " ++ show ufaInnerUnitDom
----            ++ "\n inconsistencies = " ++ show (FA.keyPointsConsistencyCheck resultReals result)
--            ++ "\n result = "
--        )
--        $
        result
        where
--        resultReals ptB = -- this is only used for consistency checking...
--            (\[x] -> x) $ FA.eval ptBOuter fOuter
--            where
--            ptBOuter =
--                DBox.insert varid fInnerVal ptB
--            fInnerVal =
--                FA.ranra2domra fInner $
--                (\[x] -> x) $ FA.eval ptB fInner
        result = ERFnDomTranslApprox ufaComp dtrBComp 
        dtrBComp = 
            DBox.union (DBox.delete varid dtrBOuter) dtrBInner
        ufaComp = 
            FA.composeNonDecreasing ufaOuter varid ufaInnerUnitDom
        ufaInnerUnitDom =
            UFA.const [dtrVarConst]
            +
            (FA.scale dtrVarSlope ufaInner)
        dtrVarSlope =
             FA.domra2ranra ufaInner $ dtrToUnitSlope dtrVar
        dtrVarConst =
             FA.domra2ranra ufaInner $ dtrToUnitConst dtrVar
        dtrVar =
            DBox.lookup "ER.RnToRm.DomTransl: composeNonDecreasing: " varid dtrBOuter

--instance 
--    (UFA.ERUnitFnApprox box varid domra ranra ufa, 
--     DomainIntBox box varid domra, 
--     VariableID varid) =>
--    UFA.ERUnitFnApprox box varid domra ranra (ERFnDomTranslApprox dtrbox varid ufa domra)
--    where
--    const vals =
--        ERFnDomTranslApprox
--        {
--            erfnUnitApprox = UFA.const vals,
--            erfnDomTransl = Map.empty
--        }
--    affine c coeffs =
--        ERFnDomTranslApprox
--        {
--            erfnUnitApprox = UFA.affine c coeffs,
--            erfnDomTransl = Map.map (const dtrIdentity) coeffs
--        }

instance 
    (UFA.ERUnitFnApprox box varid domra ranra ufa,
     RA.ERInnerOuterApprox domra, 
     DomainIntBox box varid domra,
     Show varid, Show box,
     DomainBoxMappable dtrbox box varid (DomTransl domra) domra, 
     DomainBoxMappable box dtrbox varid domra (DomTransl domra), 
     Eq dtrbox, Ord dtrbox) =>
    FA.ERFnDomApprox box varid domra ranra (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    dom (ERFnDomTranslApprox ufa dtrB) = dtrBToDomB dtrB
    bottomApprox domB tupleSize 
        | tupleSize == 1 =
            ERFnDomTranslApprox
            {
                erfnUnitApprox = UFA.bottomApprox,
                erfnDomTransl = DBox.map makeDomTransl domB
            }
    const domB vals =
        ERFnDomTranslApprox
        {
            erfnUnitApprox = UFA.const vals,
            erfnDomTransl = DBox.map makeDomTransl domB
        }
    proj domB i =
        ERFnDomTranslApprox
        {
            erfnUnitApprox = ufa,
            erfnDomTransl = domTransls
        }
        where
        domTransls = DBox.map makeDomTransl domB
        idomTransl = DBox.lookup "ERFnDomTranslApprox: ERFnDomApprox: proj: " i domTransls
        sl = FA.domra2ranra ufa $ dtrFromUnitSlope idomTransl
        co = FA.domra2ranra ufa $ dtrFromUnitConst idomTransl
        ufa = UFA.affine [co] (Map.singleton i [sl])
    -- split the function by its domain into two halves:
    bisect var maybePt f@(ERFnDomTranslApprox ufa dtrB)
        | varAbsent =
            (f, f)
        | ptOK = 
--            unsafePrint
--            (
--                "ERFnDomTranslApprox: bisect: domL = " ++ show domL ++ " domR = " ++ show domR
--            ) $
            (ERFnDomTranslApprox ufaLeft dtrLeft, 
             ERFnDomTranslApprox ufaRight dtrRight)
        | otherwise =
            error $
                "ERFnDomTranslApprox: bisect: bisection point " ++ show pt ++
                " is not exact " ++
                "(var = " ++ showVar var ++ ")" ++ 
                "(domain = " ++ show dom ++ ")"
        where
        (pt, ptOK) = 
            case maybePt of
                Just pt -> (pt, RA.isExact pt)
                Nothing -> (domM, True) 
        (domL, domM, domR, domGran) = RA.exactMiddle dom
        varAbsent = DBox.notMember var dtrB
        dom = 
            dtrDom $ DBox.lookup errMsg var dtrB
            where
            errMsg =
                "ERFnDomTranslApprox: FA.bisect: internal error: var " ++ showVar var 
                ++ " not in dtrB "
        dtrLeft = DBox.insert var (makeDomTransl domLeft) dtrB 
        dtrRight = DBox.insert var (makeDomTransl domRight) dtrB
        domLeft = domL RA.\/ pt
        domRight = pt RA.\/ domR
        (ufaLeft, ufaRight) 
            | exactTransforms =
                (UFA.composeWithThin ufa $ Map.singleton var toLeftExact,
                 UFA.composeWithThin ufa $ Map.singleton var toRightExact)
            | otherwise =
                (FA.compose ufa var toLeft,
                 FA.compose ufa var toRight)
            where
            toLeftExact =
                UFA.affine [midLeft] (Map.singleton var [slopeLeft])
            toRightExact =
                UFA.affine [midRight] (Map.singleton var [slopeRight])
            toLeft =
                (UFA.const [midLeft]) + 
                (FA.scale slopeLeft $ UFA.affine [0]  (Map.singleton var [1]))
            toRight =
                (UFA.const [midRight]) + 
                (FA.scale slopeRight $ UFA.affine [0]  (Map.singleton var [1]))
            (midLeft, slopeLeft, midRight, slopeRight, exactTransforms) =
                getTransforms 2 initGran
            initGran =
                max domGran $ RA.getGranularity pt
            getTransforms attempts gran 
                | and $ map RA.isExact [midLeft, slopeLeft, midRight, slopeRight] =
                     (midLeft, slopeLeft, midRight, slopeRight, True)
                | attempts == 0 =
                     (midLeft, slopeLeft, midRight, slopeRight, False)
                | otherwise =
                    getTransforms (attempts - 1) (gran + 1)
                where
                midLeft = slopeLeft - 1
                midRight = 1 - slopeRight
                slopeLeft = sizeLeft / size        
                slopeRight = sizeRight / size        
                size = domRgr - domLgr
                sizeLeft = ptGr - domLgr
                sizeRight = domRgr - ptGr
                domRgr = RA.setMinGranularityOuter gran $ FA.domra2ranra ufa domR
                domLgr = RA.setMinGranularityOuter gran $ FA.domra2ranra ufa domL
                ptGr = RA.setMinGranularityOuter gran $ FA.domra2ranra ufa pt

    unBisect var 
            (fL@(ERFnDomTranslApprox ufaL dtrBL),
             fR@(ERFnDomTranslApprox ufaR dtrBR)) 
        | varAbsent = fL RA./\ fR
        | otherwise = 
--            unsafePrint
--            (
--                "ERFnDomTranslApprox: unBisect: domL = " ++ show domL ++ " domR = " ++ show domR
--            ) $
            (ERFnDomTranslApprox ufa dtrB)
        where
        varAbsent = 
            DBox.notMember var dtrBL 
        dtrB = 
            DBox.insert var (makeDomTransl dom) dtrBL
        dom 
            | domLR `RA.refines` domRL =
                domL RA.\/ domR
            | otherwise =
                error $
                "ERFnDomTranslApprox: unBisect: domains do not touch: " ++ (show (domL, domR))
        (domLL, domLR) = RA.bounds domL
        (domRL, domRR) = RA.bounds domR
        domL = 
            dtrDom $ DBox.lookup errMsg var dtrBL
            where
            errMsg =
                "ERFnDomTranslApprox: unBisect: internal error: var " ++ (showVar var) 
                ++ " not in dtrBL "
        domR = 
            dtrDom $ DBox.lookup errMsg var dtrBR
            where
            errMsg =
                "ERFnDomTranslApprox: unBisect: internal error: var " ++ (showVar var)
                ++ " not in dtrBR "
        ufa = ufaLShrunk RA.\/ ufaRShrunk
        (ufaLShrunk, ufaRShrunk) 
            | exactTransforms =
                (UFA.composeWithThin ufaL $ Map.singleton var fromLExact,
                 UFA.composeWithThin ufaR $ Map.singleton var fromRExact)
            | otherwise =
                (FA.compose ufaL var fromL,
                 FA.compose ufaR var fromR)
            where
            fromLExact =
                UFA.affine [midL] (Map.singleton var [slopeL])
            fromRExact =
                UFA.affine [midR] (Map.singleton var [slopeR])
            fromL =
                (UFA.const [midL]) + 
                (FA.scale slopeL $ UFA.affine [0]  (Map.singleton var [1]))
            fromR =
                (UFA.const [midR]) + 
                (FA.scale slopeR $ UFA.affine [0]  (Map.singleton var [1]))
            (midL, slopeL, midR, slopeR, exactTransforms) =
                getTransforms 2 initGran
                where
                initGran = max (RA.getGranularity domL) (RA.getGranularity domR)
                getTransforms attempts gran 
                    | and $ map RA.isExact [midL, slopeL, midR, slopeR] =
                         (midL, slopeL, midR, slopeR, True)
                    | attempts == 0 =
                         (midL, slopeL, midR, slopeR, False)
                    | otherwise =
                        getTransforms (attempts - 1) (gran + 1)
                    where
                    midL = domRWidth / domLWidth
                    midR = - domLWidth / domRWidth
                    slopeL = midL + 1
                    slopeR = 1 - midR
                    domLWidth = domLRgran - domLLgran
                    domRWidth = domRRgran - domRLgran
                    [domLLgran, domLRgran, domRLgran, domRRgran]
                        = map setGr [domLL, domLR, domRL, domRR]
                    setGr = RA.setMinGranularityOuter gran . FA.domra2ranra ufa 
    integrate
            ix fD@(ERFnDomTranslApprox ufaD dtrBD) x integdomBox
            origin fI@(ERFnDomTranslApprox ufaInit dtrBInit) =
--        unsafePrintReturn
--        (
--            "ER.RnToRm.DomTransl: integrate: "
--            ++ "\n fD = " ++ show fD
--            ++ "\n variable = " ++ show x
--            ++ "\n origin = " ++ show origin
--            ++ "\n fI = " ++ show fI
--            ++ "\n ufaD = " ++ show ufaD
--            ++ "\n ufaDadj = " ++ show ufaDadj
--            ++ "\n originAdj = " ++ show originAdj
--            ++ "\n ufaI = " ++ show ufaI
--            ++ "\n ufaI(originAdj) = " ++ show (FA.eval (DBox.singleton x originAdj) ufaI)
--            ++ "\n result = "
--        )
--        $
        ERFnDomTranslApprox ufaI dtrBD
        where
        ufaI =
            UFA.integrate
                ix ufaDadj x 
                originAdj
                ufaInit
        ufaDadj = 
            FA.scale (FA.domra2ranra ufaD $ dtrFromUnitSlope trX) $
            ufaD
        originAdj = 
            dtrToUnit trX origin
        trX = 
            DBox.findWithDefault err x dtrBD
        err = 
            error $
                "DomTransl: faIntegrate: variable " ++ showVar x ++
                " not in the domain of the function " ++ show fD 

instance 
    (DomainBoxMappable box dtrbox varid domra (DomTransl domra),
     FA.ERFnApproxApprox box varid domra ranraa ufa) =>
     FA.ERFnApproxApprox box varid domra ranraa (ERFnDomTranslApprox dtrbox varid ufa domra)
    where
    evalAA box (ERFnDomTranslApprox ufa dtrB) =
        FA.evalAA translBox ufa
        where
        translBox = domToUnit dtrB box





