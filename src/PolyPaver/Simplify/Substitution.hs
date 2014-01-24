{-# LANGUAGE TupleSections #-}
module PolyPaver.Simplify.Substitution 
--(
--    simplifyUsingSubstitutions
--)
where

import PolyPaver.Form
import PolyPaver.Subterms
import PolyPaver.Vars (getFormFreeVars)

import qualified Data.Set as Set


simplifyUsingSubstitutions :: 
    Form TermHash -> [(Form TermHash, String)]
simplifyUsingSubstitutions form =
    {-
        1. Identify all hypotheses that are inclusions or inequalities.
        
        2. Extract possible substitutions (term pairs with monotonicity types). 
        
        3. Try to apply each substitution to the conclusion.
        
        4. Ignore substitutions that do not reduce the conclusion arity.
        
        5. Remove all hypotheses that feature a removed variable.
        
        6. Assemble the simplified conjectures, regenerating hashes.
    -}
    strongerForms
    where
    strongerForms =
        maybeFormWithoutNonConclusionVars ++
        (concat $ map buildFormFromConclusion conclusionsWithDescriptions)
        where
        maybeFormWithoutNonConclusionVars 
            | length hypotheses == length hypothesesWithOnlyConclusionVars = []
            | otherwise = [(addHashesInForm reducedForm, description)]
            where
            description =
                "removed hypotheses that contain variables not in the conclusion"
                ++ "\n\n original formula:\n"
                ++ showForm 1000000 const form  
                ++ "\n"  
                ++ "\n simplified formula:\n"
                ++ showForm 1000000 const reducedForm
                ++ "\n"
            reducedForm = 
                joinHypothesesAndConclusion (hypothesesWithOnlyConclusionVars, conclusion)
            hypothesesWithOnlyConclusionVars =
                filter (hasOnlyVars conclusionVars) hypotheses
        buildFormFromConclusion (c, reducedVars, description) 
            | length hypothesesWithoutReducedVars == length hypothesesWithOnlyConclusionVars =
                [(addHashesInForm reducedForm1, description1)]
            | otherwise =
                [(addHashesInForm reducedForm1, description1),
                 (addHashesInForm reducedForm2, description2)]
            where
            description1 =
                description ++ "; removed hypotheses that contain a reduced variable"
                ++ "\n simplified formula:\n"
                ++ showForm 1000000 const reducedForm1
                ++ "\n"
            description2 =
                description ++ "; removed hypotheses that contain variables not in the conclusion"
                ++ "\n simplified formula:\n"
                ++ showForm 1000000 const reducedForm2
                ++ "\n"
            reducedForm1 = 
                joinHypothesesAndConclusion (hypothesesWithoutReducedVars, c)
            reducedForm2 = 
                joinHypothesesAndConclusion (hypothesesWithOnlyConclusionVars, c)
            hypothesesWithOnlyConclusionVars =
                filter (hasOnlyVars $ getFormFreeVars c) hypotheses
            hypothesesWithoutReducedVars =
                filter hasNoReducedVar hypotheses -- remove hypotheses that feature a removed variable
            hasNoReducedVar h =
                Set.null $ reducedVars `Set.intersection` (getFormFreeVars h)
    hasOnlyVars vars h =
        getFormFreeVars h `Set.isSubsetOf` vars
    conclusionsWithDescriptions = 
        map addDescription $
            filter hasReducedTheArity $ -- ignoring substitutions that do not reduce arity 
                map addReducedVariables strongerConclusionsAndSubstitutions
        where
        addDescription ((c,s), reducedVars) =
            (c, reducedVars, description)
            where
            description =
                "substitution:\n" ++ showSubstitution s -- ++ "; reducedVars = " ++ show reducedVars
                ++ "\n\n original formula:\n"
                ++ showForm 1000000 const form  
                ++ "\n"  
        hasReducedTheArity (_, reducedVars) = not $ Set.null reducedVars
        addReducedVariables (c,s) = ((c,s), conclusionVars `Set.difference` getFormFreeVars c)
    strongerConclusionsAndSubstitutions =
        concat $ map (\(s, cs) -> map (,s) cs) $ zip substitutions strongerConclusionsLists 
    strongerConclusionsLists =
        -- apply each substitution onto the conclusion, ignoring those that have no effect: 
        map (makeStrengtheningSubstitution conclusion) substitutions
    -- identify substitutions from hypotheses:
    substitutions = concat $ map extractSubstitutionsFromHypothesis hypotheses
    conclusionVars = getFormFreeVars conclusion
    (hypotheses, conclusion) = getHypothesesAndConclusion form

-- Old code that tested for elimination only at the substitution level and removed only the hypothesis that
-- gave birth to the substitution:
--    strongerConclusions = concat $ map (makeStrengtheningSubstitution conclusion) substitutionsAndGaps2
--    substitutionsAndGaps2 = filter (eliminatesVar . fst) substitutionsAndGaps 
--    substitutionsAndGaps = concat $ zipWith addToAll hypothesesGaps (map extractSubstitutionsFromHypothesis hypotheses)
--    addToAll e2 l = [(e1,e2) | e1 <- l]
--    hypothesesGaps = f hypotheses []
--        where
--        f [] _ = []
--        f (h:t) p = ((reverse p) ++ t) : f t (h:p)
--    eliminatesVar subst =
--        not $ Set.null $
--        (getTermFreeVars $ subst_source subst) `Set.difference` (getTermFreeVars $ subst_target subst)
    
data Substitution =
    Substitution
    {
        subst_source :: Term TermHash,
        subst_target :: Term TermHash,
        subst_monotonicityType :: MonotonicityType
    }
    
showSubstitution :: Substitution -> String
showSubstitution substitution =
    "[" ++ sourceS ++ " -> " ++ targetS ++ "]"
    where
    sourceS = showTerm const source
    targetS = showTerm const target
    source = subst_source substitution
    target = subst_target substitution
    
data MonotonicityType = 
    Equal | Increasing | Decreasing | Expanding
    deriving (Eq) 
    
monoTypeMatches :: MonotonicityType -> MonotonicityType -> Bool
monoTypeMatches Equal _ = True
monoTypeMatches Expanding Equal = False
monoTypeMatches Expanding _ = True
monoTypeMatches mt1 mt2 = mt1 == mt2
    
monoTypeNegate :: MonotonicityType -> MonotonicityType
monoTypeNegate Increasing = Decreasing
monoTypeNegate Decreasing = Increasing
monoTypeNegate t = t
    
monoTypeBothSigns :: MonotonicityType -> MonotonicityType
monoTypeBothSigns Increasing = Equal
monoTypeBothSigns Decreasing = Equal
monoTypeBothSigns t = t
    
extractSubstitutionsFromHypothesis :: 
    Form TermHash -> [Substitution]
extractSubstitutionsFromHypothesis form =
    case form of
        Le _ l r -> substFromLess l r
        Leq _ l r -> substFromLess l r
        Ge _ l r -> substFromLess r l
        Geq _ l r -> substFromLess r l
        Eq _ l r -> substFromEq l r
        ContainedIn _ l r -> substFromContained l r
        _ -> []
    where
    substFromLess l r =
        [Substitution l r Increasing,
         Substitution r l Decreasing]
    substFromEq l r =
        [Substitution l r Equal,
         Substitution r l Equal]
    substFromContained l r =
        [Substitution l r Expanding]
    
makeStrengtheningSubstitution :: Form TermHash -> Substitution -> [Form TermHash]
makeStrengtheningSubstitution form substitution
    | formHasChanged = [addHashesInForm formS]
    | otherwise = []
    where
    Term (_, sourceHash) = subst_source substitution
    target = subst_target substitution
    availableMonoType = subst_monotonicityType substitution
    (formS, formHasChanged) = subF True form
    subF shouldStrengthen form2 =
        case form2 of
            Not arg -> 
                (Not argS, ch) 
                where 
                (argS, ch) = subF (not shouldStrengthen) arg
            Or left right ->
                (Or leftS rightS, chL || chR)
                where
                (leftS, chL) = subF shouldStrengthen left
                (rightS, chR) = subF shouldStrengthen right
            And left right ->
                (And leftS rightS, chL || chR)
                where
                (leftS, chL) = subF shouldStrengthen left
                (rightS, chR) = subF shouldStrengthen right
            Implies left right ->
                (Implies leftS rightS, chL || chR)
                where
                (leftS, chL) = subF (not shouldStrengthen) left
                (rightS, chR) = subF shouldStrengthen right
            Le lab left right ->
                (Le lab leftS rightS, chL || chR)
                where
                (leftS, chL) = subT Increasing left
                (rightS, chR) = subT Decreasing right
            Leq lab left right ->
                (Leq lab leftS rightS, chL || chR)
                where
                (leftS, chL) = subT Increasing left
                (rightS, chR) = subT Decreasing right
            Ge lab left right ->
                (Ge lab leftS rightS, chL || chR)
                where
                (leftS, chL) = subT Decreasing left
                (rightS, chR) = subT Increasing right
            Geq lab left right ->
                (Geq lab leftS rightS, chL || chR)
                where
                (leftS, chL) = subT Decreasing left
                (rightS, chR) = subT Increasing right
            Eq lab left right ->
                (Eq lab leftS rightS, chL || chR)
                where
                (leftS, chL) = subT Equal left
                (rightS, chR) = subT Equal right
            Neq lab left right ->
                (Neq lab leftS rightS, chL || chR)
                where
                (leftS, chL) = subT Equal left
                (rightS, chR) = subT Equal right
            ContainedIn lab left right -> 
                (ContainedIn lab leftS rightS, chL || chR)
                where
                (leftS, chL) = subT Expanding left
                (rightS, chR) = subT Equal right
            IsRange lab arg1 arg2 arg3 -> 
                (IsRange lab arg1S arg2S arg3S, ch1 || ch2 || ch3)
                where
                (arg1S, ch1) = subT Expanding arg1
                (arg2S, ch2) = subT Increasing arg2
                (arg3S, ch3) = subT Decreasing arg3
            IsIntRange lab arg1 arg2 arg3 -> 
                (IsIntRange lab arg1S arg2S arg3S, ch1 || ch2 || ch3)
                where
                (arg1S, ch1) = subT Equal arg1
                (arg2S, ch2) = subT Increasing arg2
                (arg3S, ch3) = subT Decreasing arg3
            IsInt lab arg -> 
                (IsInt lab argS, ch)
                where
                (argS, ch) = subT Equal arg
    subT requiredMonoType term@(Term (term', hash))
        {-
            Need to also perform a range analysis to identify some expressions
            that are always positive or always negative.
        -} 
        | hash == sourceHash && availableMonoType `monoTypeMatches` requiredMonoType = 
            (target, True)
        | otherwise =
            case term' of
                Hull left right -> subT2 requiredMonoType requiredMonoType Hull left right
                Plus left right -> subT2 requiredMonoType requiredMonoType Plus left right
                Minus left right -> 
                    subT2 requiredMonoType (monoTypeNegate requiredMonoType) Minus left right
                Neg arg -> subT1 (monoTypeNegate requiredMonoType) Neg arg
                Times left right -> 
                    subT2 (monoTypeBothSigns requiredMonoType) (monoTypeBothSigns requiredMonoType) Times left right
                Square arg -> subT1 (monoTypeBothSigns requiredMonoType) Square arg
                IntPower left right -> 
                    subT2 (monoTypeBothSigns requiredMonoType) Equal IntPower left right
                Recip arg -> subT1 (monoTypeBothSigns requiredMonoType) Recip arg
                Over left right -> 
                    subT2 (monoTypeBothSigns requiredMonoType) (monoTypeBothSigns requiredMonoType) Over left right
                Abs arg -> subT1 (monoTypeBothSigns requiredMonoType) Abs arg
                Min left right -> subT2 requiredMonoType requiredMonoType Min left right
                Max left right -> subT2 requiredMonoType requiredMonoType Max left right
                Sqrt arg -> subT1 requiredMonoType Sqrt arg
                Exp arg -> subT1 requiredMonoType Exp arg
                Sin arg -> subT1 (monoTypeBothSigns requiredMonoType) Sin arg
                Cos arg -> subT1 (monoTypeBothSigns requiredMonoType) Cos arg
                Atan arg -> subT1 requiredMonoType Atan arg
                Integral ivarId ivarName lower upper integrand ->
                    subT3 (monoTypeBothSigns requiredMonoType) (monoTypeBothSigns requiredMonoType) requiredMonoType
                        (Integral ivarId ivarName) lower upper integrand
                FRound rel abse arg -> 
                    subT1 requiredMonoType (FRound rel abse) arg
                FPlus rel abse left right -> 
                    subT2 requiredMonoType requiredMonoType (FPlus rel abse) left right
                FMinus rel abse left right -> 
                    subT2 requiredMonoType (monoTypeNegate requiredMonoType)
                        (FMinus rel abse) left right
                FTimes rel abse left right -> 
                    subT2 (monoTypeBothSigns requiredMonoType) (monoTypeBothSigns requiredMonoType) 
                        (FTimes rel abse) left right
                FSquare rel abse arg -> 
                    subT1 (monoTypeBothSigns requiredMonoType) (FSquare rel abse) arg
                FSqrt rel abse arg -> 
                    subT1 requiredMonoType (FSqrt rel abse) arg
                FSin rel abse arg -> 
                    subT1 (monoTypeBothSigns requiredMonoType) (FSin rel abse) arg
                FCos rel abse arg -> 
                    subT1 (monoTypeBothSigns requiredMonoType) (FCos rel abse) arg
                FOver rel abse left right -> 
                    subT2 (monoTypeBothSigns requiredMonoType) (monoTypeBothSigns requiredMonoType) 
                        (FOver rel abse) left right
                FExp rel abse arg -> 
                    subT1 requiredMonoType (FExp rel abse) arg
                _ -> (term, False)
        where
        subT1 mt1 constr arg1 =
            (Term (constr arg1S, 0), ch1)
            where
            (arg1S, ch1) = subT mt1 arg1
        subT2 mt1 mt2 constr arg1 arg2 =
            (Term (constr arg1S arg2S, 0), ch1 || ch2)
            where
            (arg1S, ch1) = subT mt1 arg1
            (arg2S, ch2) = subT mt2 arg2
        subT3 mt1 mt2 mt3 constr arg1 arg2 arg3 =
            (Term (constr arg1S arg2S arg3S, 0), ch1 || ch2 || ch3)
            where
            (arg1S, ch1) = subT mt1 arg1
            (arg2S, ch2) = subT mt2 arg2
            (arg3S, ch3) = subT mt3 arg3
            
            
--test1 :: [(Form TermHash, Set.Set Int)]
--test1 =
--    simplifyUsingSubstitutions $
--        addHashesInForm formEx1
--    
--formEx1 :: Form ()
--formEx1 =
--    x |>=| 1 ---> x |>=| 0
--    where
--    x = termVar 0 "x"
--    
--test2 :: [(Form TermHash, Set.Set Int)]
--test2 =
--    simplifyUsingSubstitutions $
--        addHashesInForm formEx2
--    
--formEx2 :: Form ()
--formEx2 =
--    y |<=| 1 ---> x |<-| (hull 1 y) ---> x |<-| (hull 0 (y + 1))
--    where
--    x = termVar 0 "x"
--    y = termVar 1 "y"
--    
    