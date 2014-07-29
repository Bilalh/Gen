module ToEssence where
import Language.E

class ToEssence a where
    toEssence :: a -> E

instance ToEssence EssenceLiteral where
    toEssence lit = fromEssenceLiteral lit
