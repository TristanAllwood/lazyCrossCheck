{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module LazyCrossCheck.ComparisonTable where

import Data.List
import Data.Maybe
import Data.Typeable
import Text.PrettyPrint as PP

import LazyCrossCheck.Eval
import LazyCrossCheck.Types


buildComparisonTable :: [(Exp, EvalResult)] -> [(Exp, EvalResult)]
                     -> ComparisonTable
buildComparisonTable expected actual
  = ComparisonTable . concat $ unfoldr (uncurry oneStep) (expected, actual)
  where
    oneStep :: [(Exp, EvalResult)] -> [(Exp, EvalResult)] ->
               Maybe ([ComparisonRow], ([(Exp, EvalResult)],[(Exp, EvalResult)]))
    oneStep [] []         = Nothing
    oneStep [] actuals    = let rows = map (uncurry ActualIsolated) actuals
                             in Just (rows, ([], []))
    oneStep expecteds []  = let rows = map (uncurry ExpectedIsolated) expecteds
                             in Just (rows, ([], []))
    oneStep allExpecteds@((expectedExp, expectedResult): expecteds) actuals
      | [(_, actualResult)] <- identicals
      = Just ([IdenticalExp expectedExp expectedResult actualResult],
              (expecteds, unrelateds))

      | (not . null) moreGenerals
      = Just ([ExpectedMoreGeneral expectedExp expectedResult moreGenerals],
              (expecteds, unrelateds))

      | [(mgActualExp, actualResult)] <- lessGenerals
      = let (lessGeneralExpecteds, unrelatedExpecteds)
              = partition ((==) MoreGeneral . expRelated mgActualExp . fst)
                          allExpecteds
         in Just ([ActualMoreGeneral lessGeneralExpecteds mgActualExp actualResult],
                  (unrelatedExpecteds, unrelateds)
                 )

      | otherwise = Just ( [ExpectedIsolated expectedExp expectedResult],
                           (expecteds, actuals))
      where
        actualsRelatedness = map (expRelated expectedExp . fst) actuals

        (concat -> identicals, concat -> moreGenerals,
         concat -> lessGenerals, concat -> unrelateds)
          = unzip4 $ zipWith partitionRelatedness actualsRelatedness actuals

partitionRelatedness :: Related -> a -> ([a],[a],[a],[a])
partitionRelatedness Identical    a = ([a], [], [], [])
partitionRelatedness MoreGeneral  a = ([], [a], [], [])
partitionRelatedness LessGeneral  a = ([], [], [a], [])
partitionRelatedness Unrelated    a = ([], [], [], [a])

expRelated :: Exp -> Exp -> Related
expRelated l r = argsRelated (arguments l) (arguments r)
  where
    argsRelated :: [Arg] -> [Arg] -> Related
    argsRelated ls rs = foldl combineRelated Identical $
                          zipWith argRelated ls rs

    combineRelated :: Related -> Related -> Related
    combineRelated Identical x = x
    combineRelated x Identical = x
    combineRelated MoreGeneral MoreGeneral = MoreGeneral
    combineRelated LessGeneral LessGeneral = LessGeneral
    combineRelated _ _ = Unrelated

    argRelated :: Arg -> Arg -> Related
    argRelated (ArgConstr _ cl ls) (ArgConstr _ cr rs)
      | cl == cr  = argsRelated ls rs
    argRelated (ArgConstr _ _ _) (ArgUndefined _ _)    = LessGeneral
    argRelated (ArgUndefined _ _) (ArgConstr _ _ _)    = MoreGeneral
    argRelated (ArgUndefined _ _) (ArgUndefined _ _) = Identical
    argRelated (ArgUndefined _ _) (ArgPrimitive _)   = MoreGeneral
    argRelated (ArgPrimitive l)   (ArgPrimitive r)
      | Just l' <- cast l
      , l' == r                                      = Identical
    argRelated (ArgPrimitive _)   (ArgUndefined _ _) = LessGeneral
    argRelated _ _ = Unrelated

formatRow :: String -> ComparisonRow -> Doc
formatRow name (IdenticalExp e l r)
  | l ~~> r   = PP.empty
  | otherwise = vcat [ text name <+> format e <> ": results differ."
                     , nest 2 $ vcat [ "model answer ="   <+> format l
                                     , "student answer =" <+> format r
                                     ]
                     ]
formatRow name (ExpectedMoreGeneral e l rs)
  | failedPrecondition l = PP.empty
  | otherwise
  = vcat [ text name <+> format e <> ": model answer is more general."
         , nest 2 $ vcat [ "model answer =" <+> format l
                         , "student answers:"
                         , nest 1 kids
                         ]
       ]
  where
    kids = vcat (map formatRow' rs)
    formatRow' (e', r) = text name <+> format e' <+> "=" <+> format r

formatRow name (ActualMoreGeneral ls e r)
  | null actualLines = PP.empty
  | otherwise
  = vcat [ text name <+> format e <> ": student's answer is more general."
         , nest 2 $ vcat [ "student answer =" <+> format r
                         , "model answers:"
                         , nest 1 kids
                         ]
         ]
  where
    actualLines = mapMaybe mkLine ls

    kids = vcat actualLines

    mkLine (el, l)
      | failedPrecondition l = Nothing
      | otherwise = Just $ text name <+> format el <+> "=" <+> format l

formatRow name (ExpectedIsolated e r)
  | failedPrecondition r = PP.empty
  | otherwise
  = vcat [ text name <+> format e <> ": model answer only."
         , nest 2 $ "model answer =" <+> format r
         ]

formatRow name (ActualIsolated e r)
  = vcat [ text name <+> format e <> ": student only."
         , nest 2 $ "student answer =" <+> format r
         ]
