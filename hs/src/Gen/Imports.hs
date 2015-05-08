module Gen.Imports
    ( module X
    , Depth
    , docError
    , nn
    , noteFormat
    , nub2
    , renderSized
    , renderSmall
    , prettyArr
    , prettyTypeArr
    ) where

import Conjure.Language.AbstractLiteral as X (AbstractLiteral)
import Conjure.Language.Domain          as X (Domain)
import Conjure.Language.Pretty          as X (Pretty (..))
import Conjure.Language.Type            as X
import Conjure.Prelude                  as X hiding (dropExtension)
import Control.Monad.State.Strict       as X (execStateT)
import Control.Monad.State.Strict       as X (MonadState (get, put))
import Data.Set                         as X (Set)
import Gen.AST.Imports                  as X
import Gen.Helpers.LineError            as X
import Gen.Helpers.Placeholders         as X
import GHC.Real                         as X (round)
import System.FilePath                  as X (dropExtension, dropExtensions)
import Text.Groom                       as X (groom)

import qualified Data.Set         as S
import qualified Text.PrettyPrint as Pr

type Depth = Int

-- Might want to use a hash set at some point
-- nub is O(N^2) this is O(NlogN)
nub2 :: (Ord a, Hashable a) => [a] -> [a]
nub2 l = go S.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `S.member` s then go s xs
                                      else x : go (S.insert x s) xs
-- | for printing a name and a value
-- | nn "dsd" <some Expr>
nn :: Pretty b => Doc -> b -> Doc
nn a b =  a <+> pretty b


renderSmall :: Pretty a => a -> String
renderSmall = Pr.renderStyle (Pr.style { Pr.lineLength = 120 }) . pretty

renderSized :: Pretty a => Int -> a -> String
renderSized n  = Pr.renderStyle (Pr.style { Pr.lineLength = n }) . pretty

noteFormat :: MonadIO m => Doc -> [Doc] -> m ()
noteFormat tx pr = liftIO . putStrLn . renderSized 120 $ hang tx 4 (vcat  pr)


prettyTypeArr :: [Var] -> Doc
prettyTypeArr [] = "[]"
prettyTypeArr vs = vcat $ map (\(Var a b) -> pretty (a, show b) ) vs

prettyArr :: Pretty a => [a] -> Doc
prettyArr [] = "[]"
prettyArr vs = vcat $ map pretty vs

docError :: [Doc] -> a
docError = error . show . vcat
