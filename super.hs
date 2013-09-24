{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (foldr, mapM, sequence)
import Control.Monad hiding (mapM, sequence)
import Data.Traversable




--main = (asDefault $ as putStrLn "aoeu") :: IO ()
main = sequence $ as putStrLn ["aoeu", "htns"] :: IO [()]
--main = (as (\x -> return x :: IO ()) $ (as putStrLn ["aoeu", "htns"] :: [IO ()])) :: IO [()]
--main = asDefault $ as putStrLn ["aoeu", "htns"]


-- The CanBe type class allows certain types to expose themselves as other types
-- The CanBe type class is a parametric type class taking the following types:

class CanBe input output emulator where
  type OutputEmulator input output emulator
  as :: (input -> output) -> emulator -> OutputEmulator input output emulator
--  asDefault :: b -> OutputEmulator a b a

-- any type can trivially emulate itself
instance CanBe a c a where
  type OutputEmulator a c a = c
  as func = func
--  asDefault arg = arg

-- a functor of a type can emulate that type by fmapping over itself
instance (Functor f) => CanBe a c (f a) where
  type OutputEmulator a c (f a) = f c
  as = fmap
--  asDefault arg = arg



-- this one is merely a kludge so that I don't have to use "sequence" in main
-- any iterable structure full of monadic types should be able to map and bind over itself for you
--instance (Traversable t, Monad m) => CanBe (m a) (m c) (t (m a)) where
--  type OutputEmulator (m a) (m c) (t (m a)) = m (t c)
--  type DefaultOutputEmulator a (t (m a)) (m c) = m (t a)
--  as func arg = sequence $ map func arg
--  asDefault arg = sequence arg
