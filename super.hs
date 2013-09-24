{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (foldr, mapM, sequence)
import Control.Monad hiding (mapM, sequence)
import Data.Traversable



--main = (as putStrLn "hello world") :: IO ()
main = as putStrLn ["aoeu", "htns"] :: IO [()]




-- The CanBe type class allows certain types to expose themselves to functions as other types, by taking control over how the function is executed

-- input is the type that the function expects
-- output is the type that the function returns
-- inputEmulator is the type that we are enabling to expose itself as the input type
-- outputEmulator is the type that is returned instead of output, whenever inputEmulator emulates input

class CanBe input output outputEmulator inputEmulator where
  as :: (input -> output) -> inputEmulator -> outputEmulator


-- any type can trivially emulate itself
instance CanBe a c c a where
  as func = func


-- a functor of a type can emulate that type by fmapping over itself
instance (Functor f) => CanBe a c (f c) (f a) where
  as = fmap


-- a monadic value can emulate the value it contains
instance (Monad m) => CanBe a (m c) (m c) (m a) where
  as func arg = arg >>= func



-- this one feels kind of contrived tbh, even though the implementation is dead simple
-- a function that returns a monadic value, can operate on a collection of input values
-- however, it must make sure to return a monad of the traversable, not a traversable of monads
instance (Traversable t, Monad m) => CanBe a (m c) (m (t c)) (t a) where
  as = mapM
