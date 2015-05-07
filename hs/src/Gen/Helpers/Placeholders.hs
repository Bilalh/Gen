{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
-- | This module defines placeholders that you can use while coding to
-- allow incomplete code to compile. They work just like 'undefined',
-- but with improved error messages and compile-time warnings.
-- Changed from https://github.com/ahammar/placeholders
-- quite a few changes
module Gen.Helpers.Placeholders
    ( notDone
    , todo
    , never
    , neverNote
    , PlaceholderException(..)
    ) where

import Conjure.Prelude

import Control.Exception (Exception, throw)
import Language.Haskell.TH (Q, Exp, Loc(..), litE, stringL, location, reportWarning)

-- | Thrown when attempting to evaluate a placeholder at runtime.
data PlaceholderException = PlaceholderException String
    deriving (Show, Typeable)

instance Exception PlaceholderException

-- | Indicates that this piece of code has not yet been implemented.
--
-- @$notDone = $(placeholder \"Unimplemented feature\")@
notDone :: Q Exp
notDone = placeholder "Unimplemented feature"

-- | Indicates unimplemented code or a known bug with a custom message.
--
-- @$(todo msg) = $(placeholder (\"TODO: \" ++ msg))@
todo :: String -> Q Exp
todo msg = placeholder $ "TODO: " ++ msg

-- | replacement for when undefined is needed
never :: Q Exp
never = placeholderNoWarning "Should never Happen"

-- | replacement for when undefined is needed
neverNote :: String -> Q Exp
neverNote msg = placeholderNoWarning msg

-- | Generates an expression of any type that, if evaluated at runtime will
-- throw a 'PlaceholderException'. It is therefore similar to 'error', except
-- that the source location is automatically included. Also, a warning is
-- generated at compile time so you won't forget to replace placeholders
-- before packaging your code.
placeholder :: String -> Q Exp
placeholder msg = do
    reportWarning msg
    placeholderNoWarning msg

-- | Similar to 'placeholder', but does not generate a compiler warning. Use
-- with care!
placeholderNoWarning :: String -> Q Exp
placeholderNoWarning msg = do
    runtimeMsg <- formatMessage msg `fmap` location
    [| throw $ PlaceholderException $(litE $ stringL runtimeMsg) |]

formatMessage :: String -> Loc -> String
formatMessage msg loc = msg ++ " at " ++ formatLoc loc

formatLoc :: Loc -> String
formatLoc loc = let file = loc_filename loc
                    (line, col) = loc_start loc
                in concat [file, ":", show line, ":", show col]


-- $example
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Common.Placeholders
-- >
-- > theUltimateAnswer :: Int
-- > theUltimateAnswer = $notDone
-- >
-- > main = do
-- >     putStrLn "The ultimate answer:"
-- >     print theUltimateAnswer
--
-- This will compile with a warning about the unimplemented function:
--
-- @
-- $ ghc --make Simple.hs
-- ...
-- Simple.hs:6:21: Unimplemented feature
-- ...
-- @
--
-- At runtime, an exception will be thrown when the placeholder is evaluated,
-- indicating the location of the placeholder.
--
-- @
-- $ .\/Simple
-- The ultimate answer:
-- Simple: PlaceholderExcption \"Unimplemented feature at Simple.hs:6:21\"
-- @
--
-- If compiled with the GHC flag @-Werror@, the warning will get turned into
-- an error and compilation will fail. @-Werror@ can therefore be used to
-- verify that you haven't left any unintended placeholders behind.
