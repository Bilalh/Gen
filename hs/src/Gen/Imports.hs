module Gen.Imports
    ( module X
    , docError
    , nn
    , nub2
    , renderSized
    , renderOneLine
    , prettyArr
    , prettyTypeArr
    , groomPrint
    , nnError
    , Depth
    , SpecHash
    , ParamHash
    , Time
    , Directory
    , Weight
    , logHigher2
    , logInfo2
    , logWarn2
    , logDebug2
    , logDebugVerbose2
    , hashDoc
    , fst4
    , T.Text
    ) where

-- Conjure instances and functions
import Conjure.Prelude as X (ExceptT (..), LogLevel (..), MonadFail (..),
                             MonadLog (..), allNats, getAllFiles,
                             getAllFilesWithSuffix, ignoreLogs, jsonOptions,
                             logDebug, logDebugVerbose, logInfo, logWarn, padRight,
                             pairWithContents, runLoggerPipeIO, sh, stringToText,
                             textToString,concatMapM,setRandomSeed,sortOn)

-- basic data types
import Data.Bool   as X (Bool (..), not, otherwise, (&&), (||))
import Data.Char   as X (Char, isSpace, toLower)
import Data.Int    as X (Int)
import Data.String as X (IsString (..), String)
import GHC.Enum    as X (Enum (..))
import GHC.Err     as X (error)
import GHC.Exts    as X (Double)
import GHC.Integer as X (Integer)
import GHC.Real    as X (Fractional (..), Integral (..), Real (..), fromIntegral,
                         round, truncate, (^))

-- basic type classes
import Data.Eq   as X (Eq (..))
import Data.Ord  as X (Ord (..), Ordering (..), comparing)
import GHC.Num   as X (Num (..))
import Text.Read as X (Read (..), reads)
import Text.Show as X (Show (..), showParen, showString)

-- some more type classes
import Control.Applicative as X (Applicative (..), many, optional, some, (*>), (<$>),
                                 (<*), (<|>))
import Data.Functor        as X (Functor (..))
import GHC.Generics        as X (Generic)


import Control.Arrow                as X (first, second, (&&&), (***))
import Control.Category             as X ((<<<), (>>>))
import Control.Monad                as X (Monad (return, (>>), (>>=)),
                                          MonadPlus (..), ap, filterM, foldM, guard,
                                          join, liftM, msum, mzero, replicateM,
                                          unless, void, when, zipWithM, zipWithM_,
                                          (<=<), (=<<), (>=>))
import Control.Monad.Identity       as X (Identity, runIdentity)
import Control.Monad.IO.Class       as X (MonadIO, liftIO)
import Control.Monad.Reader         as X (MonadReader (ask), ReaderT (..), asks,
                                          runReaderT)
import Control.Monad.State.Strict   as X (MonadState, MonadState (put), StateT (..),
                                          evalState, evalStateT, execStateT, get,
                                          gets, modify, runState, runStateT)
import Control.Monad.Trans.Class    as X (MonadTrans (lift))
import Control.Monad.Trans.Identity as X (IdentityT (..))
import Control.Monad.Trans.Maybe    as X (MaybeT (..), runMaybeT)
import Control.Monad.Writer.Strict  as X (MonadWriter (listen, tell),
                                          WriterT (runWriterT), execWriterT,
                                          runWriter)

import Data.Data     as X (Data, Typeable)
import Data.Default  as X (Default, def)
import Data.Either   as X (Either (..), either, lefts, partitionEithers, rights)
import Data.Function as X (const, flip, id, on, ($), (.))
import Data.List     as X (concatMap, drop, dropWhile, elem, elemIndex, filter,
                           findIndex, foldl, foldr, foldr1, genericIndex,
                           genericLength, genericTake, group, groupBy, head, init,
                           inits, intercalate, intersect, intersperse, isInfixOf,
                           isPrefixOf, isSuffixOf, last, length, lines, lookup, map,
                           minimumBy, notElem, nub, nubBy, null, partition, product,
                           repeat, replicate, reverse, sort, sortBy, span,
                           stripPrefix, subsequences, sum, tail, tails, take,
                           takeWhile, transpose, unlines, unwords, unzip, unzip3,
                           words, zip, zip3, zipWith, (++), (\\))
import Data.Maybe    as X (Maybe (..), catMaybes, fromMaybe, isJust, isNothing,
                           listToMaybe, mapMaybe, maybe, maybeToList)
import Data.Monoid   as X (Any (..), Monoid, mappend, mconcat, mempty)
import Data.Tuple    as X (curry, fst, snd, swap, uncurry)

import Data.Foldable    as X (Foldable, all, and, any, concat, fold, foldMap, forM_,
                              mapM_, maximum, minimum, or, sequence_, toList)
import Data.Set         as X (Set)
import Data.Traversable as X (Traversable, forM, mapM, sequence)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.Random as X (StdGen, mkStdGen, randomRIO, setStdGen)

-- safe
import Safe as X (at, atMay, atNote, fromJustNote, headNote, readMay, readNote)

-- hashable
import Data.Hashable as X (Hashable (..), hash)


-- aeson
import Data.Aeson as X (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)

-- pretty
import Text.PrettyPrint as X (Doc, fsep, hang, hcat, hsep, nest, punctuate, sep,
                              vcat, ($$), (<+>), (<>))

-- uniplate
import Data.Generics.Uniplate.Data as X (children, childrenBi, descend, descendBi,
                                         descendBiM, descendM, transform,
                                         transformBi, transformBiM, transformM,
                                         uniplate, universe, universeBi)

import System.Directory   as X (createDirectoryIfMissing, doesDirectoryExist,
                                doesFileExist, getDirectoryContents,
                                removeDirectoryRecursive,removeFile)
import System.Environment as X (getArgs)
import System.FilePath    as X ((</>))

import Debug.Trace as X (trace)

import System.FilePath as X (dropExtension, dropExtensions, (<.>))
import System.IO       as X (FilePath, IO, getLine, print, putStr, putStrLn,
                             writeFile)
import Text.Groom      as X (groom)


import Data.Proxy as X (Proxy (..))

import Conjure.Language.AbstractLiteral as X (AbstractLiteral)
import Conjure.Language.Constant        as X (Constant)
import Conjure.Language.Definition      as X (Expression)
import Conjure.Language.Domain          as X (Domain)
import Conjure.Language.Pretty          as X (Pretty (..))
import Conjure.Language.Type            as X

import Gen.AST.Imports          as X
import Gen.Helpers.LineError    as X
import Gen.Helpers.Placeholders as X

import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import qualified Text.PrettyPrint as Pr

type Depth     = Int
type SpecHash  = Int
type ParamHash = SpecHash
type Time      = Int
type Weight    = Int
type Directory = FilePath

instance (Pretty a, Pretty b) => Pretty (M.Map a b) where
    pretty vs = case M.toList vs of
               [] -> "[]"
               xs -> vcat $ map pretty xs


-- | nub is O(N^2) this is O(NlogN)
nub2 :: (Ord a, Hashable a) => [a] -> [a]
nub2 l = go S.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `S.member` s then go s xs
                                      else x : go (S.insert x s) xs

-- | for printing a name and a value
-- | nn "dsd" <some Expr>
nn :: Pretty b => Doc -> b -> Doc
nn a b =  a <+> pretty b <+> ""

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) =  a


renderSized :: Pretty a => Int -> a -> String
renderSized n = Pr.renderStyle (Pr.style { Pr.lineLength = n }) . pretty

renderOneLine :: Pretty a =>  a -> String
renderOneLine = Pr.renderStyle Pr.style{Pr.mode=Pr.OneLineMode} . pretty

hashDoc :: Pretty a => a -> Int
hashDoc a =
  let s = Pr.renderStyle Pr.style{Pr.mode=Pr.OneLineMode} $ pretty a
  in hash  s


prettyTypeArr :: [Var] -> Doc
prettyTypeArr [] = "⟪⟫"
prettyTypeArr vs = "⟪" <+> (vcat $ punctuate " ⋰ "
                 $ map (\(Var a b) -> pretty (a, show b) ) vs)  <+> "⟫"

prettyArr :: Pretty a => [a] -> Doc
prettyArr [] = "⟪⟫"
prettyArr vs = "⟪" <+> ( vcat $ punctuate " ⋰ " $ map pretty vs) <+> "⟫"


docError :: [Doc] -> a
docError = error . show . vcat

groomPrint :: Show a => a -> IO ()
groomPrint = putStrLn . groom

nnError :: (MonadState st m, Pretty st)  => String -> [Doc] -> m a
nnError title docs = do
  st <- get
  error . show $ ( Pr.text $ padRight 15 ' ' title  )
    Pr.$+$ (nest 4 $ vcat (docs ))
    Pr.$+$ (nest 4 $ pretty st)


logHigher2 :: MonadLog m => String -> [Doc] -> m ()
logHigher2 ln docs = log LogFollow . hang (pretty ln) 4 $ Pr.vcat docs

logInfo2 :: MonadLog m => String -> [Doc] -> m ()
logInfo2 ln docs = log LogInfo . hang (pretty ln) 4 $ Pr.vcat docs

logWarn2 :: MonadLog m => String -> [Doc] -> m ()
logWarn2 ln docs = log LogWarn . hang (pretty ln) 4 $ Pr.vcat docs

logDebug2 :: MonadLog m => String -> [Doc] -> m ()
logDebug2 ln docs = log LogDebug . hang (pretty ln) 4 $ Pr.vcat docs

logDebugVerbose2 :: MonadLog m => String -> [Doc] -> m ()
logDebugVerbose2 ln docs = log LogDebugVerbose . hang (pretty ln) 4 $ Pr.vcat docs
