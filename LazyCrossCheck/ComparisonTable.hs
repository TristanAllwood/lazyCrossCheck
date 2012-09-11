{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module LazyCrossCheck.ComparisonTable where

import Data.List
import Data.Maybe
import Text.PrettyPrint as PP

import LazyCrossCheck.Eval
import LazyCrossCheck.Types

tableStats :: ComparisonTable -> (Integer, Integer)
tableStats (ComparisonTable rows)
  = (passes, total)
  where
    passes = genericLength . filter passed $ rows
    total  = sum . map count $ rows

    passed :: ComparisonRow -> Bool
    passed (IdenticalExp _ l r)
      | failedPrecondition l = False
      | otherwise            = l ~~> r
    passed _                 = False

    count :: ComparisonRow -> Integer
    count (IdenticalExp _ l _)
      | failedPrecondition l = 0
      | otherwise            = 1

    count (ExpectedMoreGeneral _ l _)
      | failedPrecondition l = 0
      | otherwise            = 1

    count (ActualMoreGeneral ls _ _)
      = genericLength . filter (not . failedPrecondition . snd) $ ls

    count (ExpectedIsolated _ res)
      | failedPrecondition res = 0
      | otherwise              = 1

    count (ActualIsolated _ _) = 0



buildComparisonTable :: [(SimpleExp, EvalResult)] -> [(SimpleExp, EvalResult)]
                     -> ComparisonTable
buildComparisonTable expected actual
  = ComparisonTable . concat $ unfoldr (uncurry oneStep) (expected, actual)
  where
    oneStep :: [(SimpleExp, EvalResult)] -> [(SimpleExp, EvalResult)] ->
               Maybe ([ComparisonRow], ([(SimpleExp, EvalResult)],
                                        [(SimpleExp, EvalResult)]))
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

expRelated :: SimpleExp -> SimpleExp -> Related
expRelated (SimpleExp _ las) (SimpleExp _ ras)
  | length las == length ras = argsRelated las ras
  | otherwise                = Unrelated
  where
    argsRelated :: [SimpleArg] -> [SimpleArg] -> Related
    argsRelated ls rs = foldl combineRelated Identical $
                          zipWith argRelated ls rs

    combineRelated :: Related -> Related -> Related
    combineRelated Identical x = x
    combineRelated x Identical = x
    combineRelated MoreGeneral MoreGeneral = MoreGeneral
    combineRelated LessGeneral LessGeneral = LessGeneral
    combineRelated _ _ = Unrelated

    argRelated :: SimpleArg -> SimpleArg -> Related
    argRelated (SimpleConstr cl ls) (SimpleConstr cr rs)
      | cl == cr  = argsRelated ls rs
    argRelated (SimpleConstr _ _) (SimpleUndefined _)    = LessGeneral
    argRelated (SimpleUndefined _) (SimpleConstr _ _)    = MoreGeneral
    argRelated (SimpleUndefined _) (SimpleUndefined _)   = Identical
    argRelated (SimpleUndefined _) (SimplePrimitive _)   = MoreGeneral
    argRelated (SimplePrimitive l) (SimplePrimitive r)
      | l == r                                           = Identical
    argRelated (SimplePrimitive _)   (SimpleUndefined _) = LessGeneral
    argRelated _ _ = Unrelated

instance Format ComparisonRow where

  format (IdenticalExp e l r)
    | l ~~> r   = PP.empty
    | otherwise = vcat [ format e <> ": results differ."
                       , nest 2 $ vcat [ "model answer ="   <+> format l
                                       , "student answer =" <+> format r
                                       ]
                       ]
  format (ExpectedMoreGeneral e l rs)
    | failedPrecondition l = PP.empty
    | otherwise
    = vcat [ format e <> ": model answer is more general."
           , nest 2 $ vcat [ "model answer =" <+> format l
                           , "student answers:"
                           , nest 1 kids
                           ]
         ]
    where
      kids = vcat (map format' rs)
      format' (e', r) = format e' <+> "=" <+> format r

  format (ActualMoreGeneral ls e r)
    | null actualLines = PP.empty
    | otherwise
    = vcat [ format e <> ": student's answer is more general."
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
        | otherwise = Just $ format el <+> "=" <+> format l

  format (ExpectedIsolated e r)
    | failedPrecondition r = PP.empty
    | otherwise
    = vcat [ format e <> ": model answer only."
           , nest 2 $ "model answer =" <+> format r
           ]

  format (ActualIsolated e r)
    = vcat [ format e <> ": student only."
           , nest 2 $ "student answer =" <+> format r
           ]
