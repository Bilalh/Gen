{-# LANGUAGE FlexibleInstances #-}

foo :: FooType a => a
foo = bar (return ())

class FooType a where
    bar :: IO () -> a

instance FooType (IO ()) where
    bar = id

instance (Show x, FooType r) => FooType (x -> r) where
    bar s x = bar (s >> print x)
