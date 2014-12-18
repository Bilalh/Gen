{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Reduce.UnusedDomains(unusedDomains) where

import TestGen.Prelude
import qualified Data.HashSet as S

unusedDomains :: SpecE -> [Text]
unusedDomains spe = 
    case runCompESingle "f" (unusedDomainsOC (toSpec spe)  ) of
        ((Right ts),_)  -> ts
        ((Left doc),_) -> error . show . vcat $ ["unusedDomains error", doc]

--TODO don't use old conjure
unusedDomainsOC
    :: MonadConjure m
    => Spec
    -> m [Text]
unusedDomainsOC (Spec v statements) = go statements
    where
        go (StatementAndNext this next) = do
            let maybeName = case this of
                    [xMatch| [Prim (S nm)] := topLevel.declaration.find .name.reference
                           | [d]           := topLevel.declaration.find .domain
                           |] | domainNeedsRepresentation d -> Just nm
                    [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference
                           | [d]           := topLevel.declaration.given.domain
                           |] | domainNeedsRepresentation d -> Just nm
                    _ -> Nothing
            next' <- go next
            case maybeName of
                Nothing -> do
                    return next'
                Just name -> do
                    if name `S.member` identifiersIn next
                        then do
                            return next'
                        else do
                            return (name :  next')
        go p = return []

identifiersIn :: E -> S.HashSet Text
identifiersIn e =
    S.fromList [ base
               | [xMatch| [Prim (S s)] := reference |] <- universe e
               , let (base, _, _) = identifierSplit s
               ]

