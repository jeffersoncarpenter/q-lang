{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (foldr, mapM, sequence)
import Control.Monad hiding (mapM, sequence)
import Data.Traversable



-- the top two of these test main functions work if you remove the DefaultResult and asDefault members of CanBe
--main = as putStrLn "aoeu"
--main = sequence $ as putStrLn ["aoeu", "htns"] :: IO [()]
main = (as (\x -> return x :: IO ()) $ (as putStrLn ["aoeu", "htns"] :: [IO ()])) :: IO [()]
--main = asDefault $ as putStrLn ["aoeu", "htns"]


-- The CanBe type class allows certain types to expose themselves as other types, when executed in a particular context
-- a is the type to be emulated
-- b is the type that emulates a
-- c is only applicable if we are using a b to call a function that expects an a.  It is the type that the function returns
-- d is the type that the surrounding context expects
class CanBe a b c d where
  as :: (a -> c) -> b -> d

-- any type can trivially emulate itself
instance CanBe a a c c where
  as func = func
--  asDefault arg = arg

-- a functor of a type can emulate that type by fmapping over itself
instance (Functor f) => CanBe a (f a) c (f c) where
  as = fmap
--  asDefault arg = arg

-- this one is merely a kludge so that I don't have to use "sequence" in main
-- any iterable structure full of monadic types should be able to map and bind over itself for you
instance (Traversable t, Monad m) => CanBe a (t (m a)) (m c) (m (t c)) where
--  type DefaultResult a (t (m a)) (m c) = m (t a)
  as func arg = mapM (>>= func) arg
--  asDefault arg = sequence arg
