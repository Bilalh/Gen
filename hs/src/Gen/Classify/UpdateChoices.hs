module Gen.Classify.UpdateChoices(updateChoices ) where

import Conjure.Language.Definition
import Conjure.Language.Pretty
import Gen.Imports
import Gen.IO.Formats(readFromJSON,writeToJSON)


updateChoices :: FilePath -> FilePath -> IO ()
updateChoices from to = do
  items :: [QuestionAnswered] <- readFromJSON from
  let prettfied = map prettfy items

  writeToJSON to prettfied

  where
    prettfy :: QuestionAnswered -> QuestionAnswered
    prettfy AnsweredRepr{..}  = AnsweredReprStored{..}
      where aDomStored_ = renderNormal . pretty $ aDom_

    prettfy a = a

-- jsonToDoc :: ToJSON a => a -> Doc
-- jsonToDoc  = Pr.text . L.unpack . L.toLazyText . A.encodeToTextBuilder . toJSON
