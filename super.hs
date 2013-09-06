{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (foldr, mapM, sequence)
import Control.Monad hiding (mapM, sequence)
import Data.Traversable

main = as (\x -> (return x) :: IO ()) $ as putStrLn ["aoeu", "htns"]

class CanBe a b c where
  type Result a b c
  as :: (a -> c) -> b -> Result a b c
  asDefault :: b -> Result a b a -- this is equivalent to as, where the function is a no-op

instance CanBe a a c where
  type Result a a c = c
  as func = func
  asDefault arg = arg

instance (Functor f) => CanBe a (f a) c where
  type Result a (f a) c = f c
  as = fmap
  asDefault arg = arg

--instance (Monad m) => CanBe a (m a) (m c) where
--  type Result a (m a) (m c) = m c
--  as func arg = arg >>= func
--  asDefault arg = arg

instance (Traversable t, Monad m) => CanBe a (t (m a)) (m c) where
  type Result a (t (m a)) (m c) = m (t c)
  as func arg = mapM (>>= func) arg
  asDefault arg = sequence arg

--instance (Foldable f, Monad m) => CanBe (m a) (f (m a)) where
--  type Result (m a) (f (m a)) (m c) = m c
--  as func arg = foldr func (return ()) arg


--  as func arg = func (arg >> (return ()))


--sputStrLn = super putStrLn

--super :: (CanBe a c, CanBe b d) => (a -> b) -> c -> d
--super f arg = as f arg
