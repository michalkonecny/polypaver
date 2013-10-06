{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-|
    Module      :  Numeric.ER.RnToRm.Approx.DomEdges
    Description :  separate approximations per domain-box hyper-edge
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}
module Numeric.ER.RnToRm.Approx.DomEdges 
(
    ERFnDomEdges(..)
)
where

import qualified Numeric.ER.RnToRm.Approx as FA
import qualified Numeric.ER.Real.Approx as RA
import qualified Numeric.ER.Real.Approx.Elementary as RAEL

import qualified Numeric.ER.BasicTypes.DomainBox as DBox
import Numeric.ER.BasicTypes.DomainBox (VariableID(..), DomainBox)
import Numeric.ER.BasicTypes
import Numeric.ER.Misc
import Numeric.ER.BasicTypes.PlusMinus

import Numeric.ER.ShowHTML
import qualified Text.Html as H

import Data.Typeable
import Data.Generics.Basics
import Data.Binary

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

{-|
    Use some function approximation type and for each domain box
    keep a structure of function approximations of this type indexed
    by the hyper-edge structure.  For each hyper-edge of the domain,
    the approximation has this edge as its domain.
    
    E.g. for a 2D square domain there are:
    
      * one approximation for the whole square
      
      * four 1D approximations, one for each edge
      
      * eight 0D approximations, one for each endpoint of each edge 
 -}
data ERFnDomEdges varid fa =
    ERFnDomEdges
    {
        erfnMainVolume :: fa,
        erfnEdges :: Map.Map (varid, PlusMinus) (ERFnDomEdges varid fa)
    }
    deriving (Typeable,Data)

instance (Ord a, Binary a, Binary b) => Binary (ERFnDomEdges a b) where
  put (ERFnDomEdges a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (ERFnDomEdges a b)

edgesLift1 ::
    (fa -> fa) ->
    (ERFnDomEdges varid fa) -> (ERFnDomEdges varid fa)
edgesLift1 op (ERFnDomEdges mainEncl edges) =    
    ERFnDomEdges (op mainEncl) (Map.map (edgesLift1 op) edges)
        
edgesLift2 ::
    (Ord varid) =>
    (fa -> fa -> fa) ->
    (ERFnDomEdges varid fa) -> (ERFnDomEdges varid fa) -> (ERFnDomEdges varid fa)
edgesLift2 op f1@(ERFnDomEdges mainEncl1 edges1) f2@(ERFnDomEdges mainEncl2 edges2) 
        | Map.keys edges1 == Map.keys edges2 =
            ERFnDomEdges (mainEncl1 `op` mainEncl2) $
                Map.intersectionWith (edgesLift2 op) edges1 edges2
        | otherwise =
            edgesLift2 op f1a f2a
        where
        (f1a, f2a) = unifyEdgeVariables f1 f2

unifyEdgeVariables ::
    (Ord varid) =>
    ERFnDomEdges varid fa ->
    ERFnDomEdges varid fa ->
    (ERFnDomEdges varid fa, ERFnDomEdges varid fa)
unifyEdgeVariables 
        f1@(ERFnDomEdges fa1 edges1) 
        f2@(ERFnDomEdges fa2 edges2) =
    (ERFnDomEdges fa1 edges1amended, 
     ERFnDomEdges fa2 edges2amended)
    where
    vars1 = Set.map fst $ Map.keysSet edges1
    vars2 = Set.map fst $ Map.keysSet edges2
    vars = Set.union vars1 vars2
    newVars1 = vars2 `Set.difference` vars1 
    newVars2 = vars1 `Set.difference` vars2 
    (ERFnDomEdges _ edges1amended) = 
        foldl (\f v -> addVarToEdges v f) f1 $ Set.toList newVars1
    (ERFnDomEdges _ edges2amended) = 
        foldl (\f v -> addVarToEdges v f) f2 $ Set.toList newVars2

addVarToEdges ::
    (Ord varid) =>
    varid ->
    ERFnDomEdges varid fa ->
    ERFnDomEdges varid fa 
addVarToEdges var f@(ERFnDomEdges fa edges) =
    (ERFnDomEdges fa edgesNew)
    where
    edgesNew =
        Map.insert (var, Plus) f $ 
            Map.insert (var, Minus) f $ 
                Map.map (addVarToEdges var) edges


instance 
    (FA.ERFnDomApprox box varid domra ranra fa, Ord varid, VariableID varid) =>
    Show (ERFnDomEdges varid fa)
    where
    show f@(ERFnDomEdges fa edges) =
        showAux [] f
        where
        showAux varAssignments (ERFnDomEdges fa edges) =
            edgeDescription ++
            show fa ++
            (concat $ map showEdge $ Map.toList edges)
            where
            edgeDescription 
                | null varAssignments =
                    "\n>>>>> main enclosure: "
                | otherwise =
                    "\n>>>>> edge" ++ showVarAssignments varAssignments ++ ": "
            showVarAssignments varAssignments =
                concat $ map showVarAssignment $ reverse varAssignments
            showVarAssignment (varID, val) =
                " " ++ showVar varID ++ "=" ++ show val
            showEdge ((varId, pm), faEdge) =
                showAux ((varId, varDomEndpoint) : varAssignments) faEdge
                where 
                varDomEndpoint =
                    case pm of
                        Minus -> varDomLo
                        Plus -> varDomHi 
                (varDomLo, varDomHi) = RA.bounds varDom
                varDom = DBox.findWithDefault RA.bottomApprox varId domB
        domB = FA.dom fa

instance
    (FA.ERFnDomApprox box varid domra ranra fa, H.HTML fa) =>
    H.HTML (ERFnDomEdges varid fa)
    where
    toHtml f@(ERFnDomEdges mainEncl edges) 
        | Map.null edges =
            H.toHtml mainEncl
        | otherwise =
            H.toHtml $
                abovesTable [H.border 2] [leftEdgesHtml, H.toHtml mainEncl, rightEdgesHtml]
        where
        leftEdgesHtml = 
            H.toHtml $ H.simpleTable [H.border 1] [] leftRows
        rightEdgesHtml = 
            H.toHtml $ H.simpleTable [H.border 1] [] rightRows
        leftRows =
            map (makeRow fst) leftEdges
        rightRows =
            map (makeRow snd) rightEdges
        (leftEdges, rightEdges) =
             partition (\((_, pm),_) -> pm == Minus) $ Map.toList edges
        makeRow pickEndpoint ((varId, _), faEdge) =
            [
                H.stringToHtml $ showVar varId ++ "=" ++ show varValueLeft
            ,
                H.toHtml faEdge
            ]
            where
            varValueLeft = 
                pickEndpoint $ 
                RA.bounds $ DBox.lookup "RnToRm.Approx.DomEdges: " varId domB
        domB = FA.dom mainEncl
        
instance
    (FA.ERFnApprox box varid domra ranra fa) =>
    Eq (ERFnDomEdges varid fa)
    where
    (ERFnDomEdges fa1 edges1) == (ERFnDomEdges fa2 edges2) =
        fa1 == fa2

instance
    (FA.ERFnApprox box varid domra ranra fa, Ord fa) =>
    Ord (ERFnDomEdges varid fa)
    where
    compare (ERFnDomEdges fa1 edges1) (ERFnDomEdges fa2 edges2) =
        compare fa1 fa2

instance
    (FA.ERFnDomApprox box varid domra ranra fa, VariableID varid) =>
    Num (ERFnDomEdges varid fa)
    where
    fromInteger n = ERFnDomEdges (fromInteger n) Map.empty
    negate = edgesLift1 negate
    (+) = edgesLift2 (+)
    (*) = edgesLift2 (*)

instance 
    (FA.ERFnDomApprox box varid domra ranra fa, VariableID varid) =>
    Fractional (ERFnDomEdges varid fa)
    where
    fromRational r = ERFnDomEdges (fromRational r) Map.empty
    recip = edgesLift1 recip 


instance 
    (FA.ERFnDomApprox box varid domra ranra fa, VariableID varid, Show box) =>
    RA.ERApprox (ERFnDomEdges varid fa)
    where
    initialiseBaseArithmetic _ =
    	RA.initialiseBaseArithmetic (0 :: fa)
    getGranularity (ERFnDomEdges mainEncl edges) =
        RA.getGranularity mainEncl
    setGranularityOuter gran = edgesLift1 (RA.setGranularityOuter gran) 
    setMinGranularityOuter gran = edgesLift1 (RA.setMinGranularityOuter gran)
    isBounded (ERFnDomEdges mainEncl edges) =
        RA.isBounded mainEncl -- may need to check edges too?
    f1 /\ f2 = edgesLift2 (RA./\) f1 f2
    intersectMeasureImprovement ix 
            f1@(ERFnDomEdges mainEncl1 edges1) 
            f2@(ERFnDomEdges mainEncl2 edges2) 
        | Map.keys edges1 == Map.keys edges2 =
            (ERFnDomEdges mainEnclIsect edgesIsect,
             ERFnDomEdges mainEnclImpr edgesImpr)
        | otherwise =
            RA.intersectMeasureImprovement ix f1a f2a
        where
        (f1a, f2a) = unifyEdgeVariables f1 f2
        (mainEnclIsect, mainEnclImpr) =
             RA.intersectMeasureImprovement ix mainEncl1 mainEncl2
        edgesIsect = Map.map fst edgesIsectImpr
        edgesImpr = Map.map snd edgesIsectImpr
        edgesIsectImpr =
            Map.intersectionWith (RA.intersectMeasureImprovement ix) edges1 edges2 
    leqReals fa1 fa2 =
        RA.leqReals (erfnMainVolume fa1) (erfnMainVolume fa2)
    refines
            f1@(ERFnDomEdges mainEncl1 edges1) 
            f2@(ERFnDomEdges mainEncl2 edges2) =
--        unsafePrint 
--        (
--            "ERFnDomEdges: refines: "
--            ++ "\n domB = " ++ show (FA.dom mainEncl1) 
--            ++ "\n mainRefines = " ++ show mainRefines 
--            ++ "\n mainEncl1 = " ++ show mainEncl1 
--            ++ "\n mainEncl2 = " ++ show mainEncl2 
--        )$
        mainRefines && edgesRefine
        where
        mainRefines =
            mainEncl1 `RA.refines` mainEncl2
        edgesRefine =
            Map.fold (&&) True $
                Map.intersectionWith RA.refines edges1 edges2  
    compareApprox
            f1@(ERFnDomEdges mainEncl1 edges1) 
            f2@(ERFnDomEdges mainEncl2 edges2) =
        compareComposeMany
        [
            RA.compareApprox mainEncl1 mainEncl2
        ,
            compareListsWith compareEdges (Map.toAscList edges1) (Map.toAscList edges2)
        ]
        where
        compareEdges ((v1,pm1),fa1) ((v2,pm2),fa2) =
            compareComposeMany
            [
                compare v1 v2,
                compare pm1 pm2,
                RA.compareApprox fa1 fa2
            ]
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RA.ERIntApprox fa, 
     VariableID varid, Show box) =>
    RA.ERIntApprox (ERFnDomEdges varid fa)
    where
--    doubleBounds = :: ira -> (Double, Double) 
--    floatBounds :: ira -> (Float, Float)
--    integerBounds :: ira -> (ExtendedInteger, ExtendedInteger)
    bisectDomain maybePt (ERFnDomEdges mainEncl edges) =
        (ERFnDomEdges mainEnclLo edgesLo,
         ERFnDomEdges mainEnclHi edgesHi)
        where
        (mainEnclLo, mainEnclHi) = RA.bisectDomain maybePtMainEncl mainEncl
        edgesLoHi = Map.intersectionWith RA.bisectDomain maybePtEdges edges
        edgesLo = Map.map fst edgesLoHi 
        edgesHi = Map.map snd edgesLoHi 
        (maybePtMainEncl, maybePtEdges) =
            case maybePt of
                Nothing -> 
                    (Nothing, 
                     Map.map (const Nothing) edges)
                Just (ERFnDomEdges mainEnclPt edgesPt) ->
                    (Just mainEnclPt,
                     Map.map Just edgesPt)
    bounds (ERFnDomEdges mainEncl edges) =
        (ERFnDomEdges mainEnclLo edgesLo,
         ERFnDomEdges mainEnclHi edgesHi)
        where
        (mainEnclLo, mainEnclHi) = RA.bounds mainEncl
        edgesLoHi = Map.map (RA.bounds) edges
        edgesLo = Map.map fst edgesLoHi 
        edgesHi = Map.map snd edgesLoHi
    f1 \/ f2 = edgesLift2 (RA.\/) f1 f2

instance 
    (FA.ERFnDomApprox box varid domra ranra fa, RAEL.ERApproxElementary fa, 
     VariableID varid, Show box) =>
    RAEL.ERApproxElementary (ERFnDomEdges varid fa)
    where
    abs ix = edgesLift1 $ RAEL.abs ix
    sqrt ix = edgesLift1 $ RAEL.sqrt ix
    exp ix = edgesLift1 $ RAEL.exp ix
    log ix = edgesLift1 $ RAEL.log ix
    sin ix = edgesLift1 $ RAEL.sin ix
    cos ix = edgesLift1 $ RAEL.cos ix
    atan ix = edgesLift1 $ RAEL.atan ix
        
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, 
     VariableID varid, Show box) =>
    FA.ERFnApprox box varid domra ranra (ERFnDomEdges varid fa)
    where
    check prgLocation (ERFnDomEdges mainEncl edges) =
        ERFnDomEdges 
            (FA.check prgLocation mainEncl) 
            (Map.mapWithKey checkEdge edges)
        where
        checkEdge (var, pm) edgeFA =
            FA.check (prgLocation ++ showVar var ++ show pm ++ ": ") edgeFA
    domra2ranra fa d =
        FA.domra2ranra (erfnMainVolume fa) d
    ranra2domra fa r =
        FA.ranra2domra (erfnMainVolume fa) r
    setMaxDegree maxDegree = edgesLift1 (FA.setMaxDegree maxDegree)
    setMaxSize maxSize = edgesLift1 (FA.setMaxSize maxSize)
    getRangeApprox (ERFnDomEdges mainEncl _) =
        FA.getRangeApprox mainEncl
    getTupleSize (ERFnDomEdges mainEncl _) =
        FA.getTupleSize mainEncl
    tuple [] = error "ERFnDomEdges: FA.tuple: empty list"
    tuple fs =
        foldl1 consFs fs 
        where
        consFs = edgesLift2 $ \a b -> FA.tuple [a,b]
    applyTupleFn tupleFn fn = (edgesLift1 $ FA.applyTupleFn tupleFnNoEdges) fn
        where
        tupleFnNoEdges fas =
            map erfnMainVolume $
                tupleFn $
                    map (\fa -> ERFnDomEdges fa (makeEdges fa (erfnEdges fn))) 
                        fas
        makeEdges fa oldEdges =
            Map.mapWithKey (makeVarPMEdge fa) oldEdges
        makeVarPMEdge fa (var, pm) oldEdge =
            ERFnDomEdges faNoVar $ makeEdges faNoVar (erfnEdges oldEdge)
            where
            faNoVar =
                FA.partialEval (DBox.singleton var domEndPt) fa
            domEndPt =
                case pm of Minus -> domL; Plus -> domR
            (domL, domR) = RA.bounds dom
            [dom] = DBox.elems $ FA.dom fa
    volume (ERFnDomEdges mainEncl edges) = FA.volume mainEncl
    scale ratio = edgesLift1 (FA.scale ratio)
    partialIntersect ix substitutions 
            f1@(ERFnDomEdges mainEncl1 edges1) 
            f2@(ERFnDomEdges mainEncl2 edges2) 
        | Map.keys edges1 == Map.keys edges2 =
            ERFnDomEdges (FA.partialIntersect ix substitutions mainEncl1 mainEncl2) $
                Map.intersectionWithKey partialIntersectEdge edges1 edges2
        | otherwise =
            FA.partialIntersect ix substitutions f1a f2a
        where
        (f1a, f2a) = unifyEdgeVariables f1 f2
        partialIntersectEdge (var, pm) edge1 edge2 
            | withinSubstitutions =
                FA.partialIntersect ix substitutions edge1 edge2
            | otherwise = edge2
            where
            withinSubstitutions =
                (varDomEndpoint pm) `RA.refines` varVal
                where
                varVal =
                    DBox.findWithDefault RA.bottomApprox var substitutions
            varDomEndpoint Minus = varDomLO
            varDomEndpoint Plus = varDomHI
            (varDomLO, varDomHI) = RA.bounds varDom
            varDom = DBox.lookup "DomEdges: partialIntersect: " var $ FA.dom mainEncl2 
    eval ptBox (ERFnDomEdges mainEncl edges) 
        | null edgeVals =
            mainVal
        | otherwise =
            foldl1 (zipWith (RA./\)) edgeVals
        where
        mainVal = FA.eval ptBox mainEncl
        edgeVals = 
            concat $ map edgeEval $ Map.toList edges
        edgeEval ((x, sign), edgeFA) 
            | xPt `RA.refines` xDomLo && sign == Minus =
                [FA.eval ptBoxNoX edgeFA]
            | xPt `RA.refines` xDomHi && sign == Plus =
                [FA.eval ptBoxNoX edgeFA]
            | otherwise = []
            where
            (xDomLo, xDomHi) = RA.bounds xDom
            xDom = DBox.findWithDefault RA.bottomApprox x $ FA.dom mainEncl
            xPt = DBox.findWithDefault RA.bottomApprox x ptBox
            ptBoxNoX = DBox.delete x ptBox
            
    evalInner ptBox (ERFnDomEdges mainEncl edges) 
        | null edgeVals =
            mainVal
        | otherwise =
            foldl1 (zipWith (RA./\)) edgeVals
        where
        mainVal = FA.evalInner ptBox mainEncl
        edgeVals = 
            concat $ map edgeEval $ Map.toList edges
        edgeEval ((x, sign), edgeFA) 
            | xPt `RA.refines` xDomLo && sign == Minus =
                [FA.evalInner ptBoxNoX edgeFA]
            | xPt `RA.refines` xDomHi && sign == Plus =
                [FA.evalInner ptBoxNoX edgeFA]
            | otherwise = []
            where
            (xDomLo, xDomHi) = RA.bounds xDom
            xDom = DBox.lookup "DomEdges: evalInner: xDom: "  x $ FA.dom mainEncl
            xPt = DBox.findWithDefault RA.bottomApprox x ptBox
            ptBoxNoX = DBox.delete x ptBox
            
    partialEval substitutions f@(ERFnDomEdges mainEncl edges) =
        (ERFnDomEdges mainEnclSubst edgesSubst)
        where
        mainEnclSubst = FA.partialEval substitutions mainEnclSelect
        edgesSubst = 
            Map.map (FA.partialEval substitutionsSelect) $
            Map.filterWithKey (\ (varID,_) _ -> varID `DBox.notMember` substitutionsSelect) edgesSelect
        (ERFnDomEdges mainEnclSelect edgesSelect, substitutionsSelect) = 
            foldl selectVar (f, substitutions) $ DBox.toList substitutions
        selectVar (fPrev@(ERFnDomEdges _ edgesPrev), substitutionsPrev) (varID, varVal)
            | varVal `RA.refines` varDomLo =
                case Map.lookup (varID, Minus) edgesPrev of
                    Nothing -> (fPrev, substitutionsPrev)
                    Just fSelect -> (fSelect, substitutionsNew) 
            | varVal `RA.refines` varDomHi =
                case Map.lookup (varID, Plus) edgesPrev of
                    Nothing -> (fPrev, substitutionsPrev)
                    Just fSelect -> (fSelect, substitutionsNew) 
            | otherwise = (fPrev, substitutionsPrev)
            where
            (varDomLo, varDomHi) = RA.bounds varDom
            varDom = DBox.findWithDefault RA.bottomApprox varID $ FA.dom mainEncl
            substitutionsNew = DBox.delete varID substitutionsPrev
            
instance 
    (FA.ERFnDomApprox box varid domra ranra fa, 
     VariableID varid, Show box, Show varid) =>
    FA.ERFnDomApprox box varid domra ranra (ERFnDomEdges varid fa)
    where
    dom (ERFnDomEdges mainEncl edges) = FA.dom mainEncl
    bottomApprox domB tupleSize =
        ERFnDomEdges (FA.bottomApprox domB tupleSize) $
            Map.fromList $ concat $
                map varEdges $ DBox.toList domB
        where 
        varEdges (varId, _) =
            [((varId, Minus), fEdge), ((varId, Plus), fEdge)]
            where
            fEdge = 
                FA.bottomApprox (DBox.delete varId domB) tupleSize
    const domB vals =
        ERFnDomEdges (FA.const domB vals) $
            Map.fromList $ concat $
                map varEdges $ DBox.toList domB
        where 
        varEdges (varId, _) =
            [((varId, Minus), fEdge), ((varId, Plus), fEdge)]
            where
            fEdge = 
                FA.const (DBox.delete varId domB) vals
    proj domB i =
        ERFnDomEdges mainEncl edges
--            Nothing ->
--                error $ 
--                    "DomEdges: projection index " ++ show i 
--                    ++ " out of range for domain " ++ show domB
        where
        mainEncl = FA.proj domB i
        edges =
            Map.fromList $ concat $ map makeVarEdges $ DBox.toList domB
        makeVarEdges (varID, varDom)
            | i == varID =
                [((varID, Minus), FA.const domNoVar [FA.domra2ranra mainEncl idomLo]),
                 ((varID, Plus), FA.const domNoVar [FA.domra2ranra mainEncl idomHi])]
            | otherwise =
                [((varID, Minus), faNoVar),
                 ((varID, Plus), faNoVar)]
            where
            domNoVar = DBox.delete varID domB
            (idomLo, idomHi) = RA.bounds idom
            idom = DBox.lookup "DomEdges: FA.proj: " i domB
            faNoVar = FA.proj domNoVar i
    bisect var maybePt f@(ERFnDomEdges mainEncl edges) 
        | varAbsent = (f,f)
        | otherwise =
            (ERFnDomEdges mainEnclLo edgesLo,
             ERFnDomEdges mainEnclHi edgesHi)
        where
        varAbsent =
            Map.notMember (var, Minus) edges
        (mainEnclLo, mainEnclHi) = FA.bisect var maybePt mainEncl
        pt = 
            case maybePt of 
                Nothing -> RA.defaultBisectPt varDom
                Just pt -> pt
            where
            varDom = 
                DBox.findWithDefault RA.bottomApprox var $ FA.dom mainEncl
        edgesLo =
            Map.insert (var, Minus) (edges Map.! (var, Minus)) $
            Map.insert (var, Plus) fAtPt $
            edgesLoNoVar
        edgesHi =
            Map.insert (var, Minus) fAtPt $
            Map.insert (var, Plus) (edges Map.! (var, Plus)) $
            edgesHiNoVar
        fAtPt = FA.partialEval (DBox.singleton var pt) f
        edgesLoNoVar = Map.map fst edgesLoHiNoVar
        edgesHiNoVar = Map.map snd edgesLoHiNoVar
        edgesLoHiNoVar = 
            Map.map (FA.bisect var maybePt) edgesNoVar
        edgesNoVar = 
            Map.delete (var, Plus) $ Map.delete (var, Minus) edges

    integrate ix fD x integdomBox origin fInit =
        deriveInfoForXEdges $
            integrateNoXEdges
                fDNoXUnif fInitUnif
        where
        (fDNoXUnif, fInitUnif) = unifyEdgeVariables fDNoX fInit
            where
            fDNoX = ERFnDomEdges mainEnclD edgesDNoX
            edgesDNoX = Map.delete (x,Plus) $ Map.delete (x,Minus) $ edgesD
            (ERFnDomEdges mainEnclD edgesD) = fD
        integrateNoXEdges 
                fD@(ERFnDomEdges mainEnclD edgesD) 
                fI@(ERFnDomEdges mainEnclI edgesI) =
            ERFnDomEdges mainEncl edges
            where
            mainEncl = FA.integrate ix mainEnclD x integdomBox origin mainEnclI
            edges = Map.intersectionWithKey integrEdge edgesD edgesI
                where
                integrEdge (varID, _) edgeD edgeI =
                    FA.integrate ix edgeD x (DBox.delete varID integdomBox) origin edgeI
        deriveInfoForXEdges fNoX@(ERFnDomEdges mainEncl edgesNoX) = 
--            | 2 == (length $ DBox.toList $ FA.dom mainEncl) =
--                unsafePrint
--                (
--                    "DomEdges: integrate: deriveInfoForXEdges: "
--                    ++ "\n fNoX = " ++ show fNoX
--                    ++ "\n FA.partialEval (DBox.singleton x xDomLo) fNoX = " 
--                        ++ show (FA.partialEval (DBox.singleton x xDomLo) fNoX)
--                ) $
--                ERFnDomEdges mainEncl edges
--            | otherwise = 
            ERFnDomEdges mainEncl edges 
            where
            edges = 
                Map.insert (x, Minus) (FA.partialEval (DBox.singleton x xDomLo) fNoX) $ 
                Map.insert (x, Plus) (FA.partialEval (DBox.singleton x xDomHi) fNoX) $
                edgesNoX
        (xDomLo, xDomHi) = RA.bounds xDom
            where
            xDom = 
                DBox.findWithDefault 
                    (error $ "DomEdges: integrate: edgesInit: no variable " ++ show x) 
                    x $ FA.dom fD
--            
--            
--        ERFnDomEdges mainEncl edges
--        where
--        (ERFnDomEdges mainEnclD edgesD, 
--         ERFnDomEdges _mainEnclInitWithX edgesInitWithX) = unifyEdgeVariables fD fInit
--        mainEncl = 
--            FA.integrate ix mainEnclD x integdomBox origin mainEnclInit
--            where
--            (ERFnDomEdges mainEnclInit _) = fInit
--        edges = 
--            unsafePrint
--            (
--                "DomEdges: integrate: edges: "
--                ++ "\n fNoX = " ++ show fNoX
----                ++ "\n (FA.partialEval (DBox.singleton x xDomLo) fNoX) = " 
----                        ++ show (FA.partialEval (DBox.singleton x xDomLo) fNoX)
--            )$
--            Map.insert (x, Minus) (FA.partialEval (DBox.singleton x xDomLo) fNoX) $ 
--            Map.insert (x, Plus) (FA.partialEval (DBox.singleton x xDomHi) fNoX) $
--            edgesNoX
--            where
--            edgesNoX =
--                Map.intersectionWithKey integrEdge edgesDNoX edgesInit
--                where
--                edgesDNoX = 
--                    Map.delete (x,Plus) $ Map.delete (x,Minus) $ edgesD
--                (ERFnDomEdges _ edgesInit) = 
--                    Map.findWithDefault (error $ "DomEdges: integrate: edgesInit: no variable " ++ show x) 
--                        (x, Minus) edgesInitWithX
--                integrEdge (varID, _) edgeD edgeInit =
--                    FA.integrate ix edgeD x (DBox.delete varID integdomBox) origin edgeInit
--            fNoX = ERFnDomEdges mainEncl edgesNoX
--            (xDomLo, xDomHi) = RA.bounds xDom
--                where
--                xDom = 
--                    DBox.findWithDefault 
--                        (error $ "DomEdges: integrate: edgesInit: no variable " ++ show x) 
--                        x $ FA.dom fD
            
    integrateMeasureImprovement ix fD x integdomBox xOrigin fP =
--        unsafePrint 
--            ("DomEdges: integrateMeasureImprovement: faIntegrLo = " ++ show faIntegrLo)  
        (faIntegr, faImprovement)
        where
        faIntegr =
            faIntegrIsect
--            case RA.compareReals (FA.volume faIntegrIsect) (FA.volume faIntegrRaw) of
--                Just LT -> faIntegrIsect
--                _ -> faIntegrRaw -- this is wrong - forgets initial conditions!
        (faIntegrIsect, faImprovement) = 
            RA.intersectMeasureImprovement ix fP faIntegrRaw
        faIntegrRaw 
            | RA.isExact xOrigin = faIntegrLo
            | otherwise = faIntegrLo RA./\ faIntegrHi
        (xOriginLo, xOriginHi) = RA.bounds xOrigin
        faIntegrLo = 
            FA.integrate ix fD x integdomBox xOriginLo faPxLo      
        faPxLo = 
            FA.partialEval (DBox.singleton x xOriginLo) fP 
        faIntegrHi = 
            FA.integrate ix fD x integdomBox xOriginHi faPxHi      
        faPxHi = 
            FA.partialEval (DBox.singleton x xOriginHi) fP 
         

