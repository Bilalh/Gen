{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestGen.Reduce.UnusedDomains(unusedDomains) where

import TestGen.Prelude
import qualified Data.HashSet as S

unusedDomains :: SpecE -> [Text]
unusedDomains spe =
    case runCompESingle "f" (unusedDomainsOC (toSpec spe)  ) of
        ((Right ts),_) -> ts
        ((Left doc),lg) -> error . show . vcat $ ["unusedDomains rrError", doc,pretty lg]

--TODO don't use old conjure
unusedDomainsOC
    :: MonadConjure m
    => Spec
    -> m [Text]
unusedDomainsOC (Spec _ statements) = go statements
    where
        go (StatementAndNext this next) = do
            let maybeName = case this of
                    [xMatch| [Prim (S nm)] := topLevel.declaration.find.name.reference
                           | [_]           := topLevel.declaration.find.domain
                           |] -> Just nm
                    _ -> Nothing
            next' <- go next
            case maybeName of
                Nothing -> do
                    return next'
                Just name -> do
                    if name == "unused" || name `S.member` identifiersIn next
                        then do
                            return next'
                        else do
                            return (name :  next')
        go _ = return []

identifiersIn :: E -> S.HashSet Text
identifiersIn e =
    S.fromList [ base
               | [xMatch| [Prim (S s)] := reference |] <- universe e
               , let (base, _, _) = identifierSplit s
               ]
