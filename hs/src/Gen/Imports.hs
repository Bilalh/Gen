module Gen.Imports
    ( module X
    , docError
    , nn
    , noteFormat
    , nub2
    , renderSized
    , renderSmall
    , prettyArr
    , prettyTypeArr
    , groomPrint
    , nnError
    , Depth
    , Hash
    , Time
    , Directory
    , Weight
    , logHigher2
    , logInfo2
    , logWarn2
    , logDebug2
    , logDebugVerbose2
    ) where

import Conjure.Language.AbstractLiteral as X (AbstractLiteral)
import Conjure.Language.Domain          as X (Domain)
import Conjure.Language.Pretty          as X (Pretty (..))
import Conjure.Language.Constant        as X (Constant)
import Conjure.Language.Definition      as X (Expression)
import Conjure.Language.Type            as X
import Conjure.Prelude                  as X hiding (dropExtension)
import Control.Monad.State.Strict       as X (execStateT)
import Control.Monad.State.Strict       as X (MonadState (get, put))
import Data.Set                         as X (Set)
import Gen.AST.Imports                  as X
import Gen.Helpers.LineError            as X
import Gen.Helpers.Placeholders         as X
import GHC.Real                         as X (round,truncate)
import System.FilePath                  as X (dropExtension, dropExtensions, (<.>))
import Text.Groom                       as X (groom)
import qualified Data.Map.Strict as M

import qualified Data.Set         as S
import qualified Text.PrettyPrint as Pr

type Depth     = Int
type Hash      = Int
type Time      = Int
type Weight    = Int
type Directory = FilePath

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


renderSmall :: Pretty a => a -> String
renderSmall = Pr.renderStyle (Pr.style { Pr.lineLength = 120 }) . pretty

renderSized :: Pretty a => Int -> a -> String
renderSized n  = Pr.renderStyle (Pr.style { Pr.lineLength = n }) . pretty

noteFormat :: MonadIO m => Doc -> [Doc] -> m ()
noteFormat tx pr = liftIO . putStrLn . renderSized 120 $ hang tx 4 (vcat  pr)


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

instance (Pretty a, Pretty b) => Pretty (M.Map a b) where
    pretty vs = case M.toList vs of
               [] -> "[]"
               xs -> vcat $ map pretty xs


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
