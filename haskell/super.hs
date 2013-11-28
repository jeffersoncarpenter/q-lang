{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, ExistentialQuantification, Rank2Types, UndecidableInstances #-}

import Prelude hiding (foldr, mapM, sequence)
import Control.Monad hiding (mapM, sequence)
import Data.Traversable


-- have to include type signatures with every usage, which is annoying

main = (sputStrLn "aoeu"             :: IO ()) >>
       (sputStrLn ["Hello", "World"] :: IO [()]) >>
       (sputStrLn "Enter some text:" :: IO ()) >>
       (sputStrLn getLine            :: IO ())


-- SType t () = t
-- SType t (f ()) = f t
-- SType t (g (f ())) = g (f t)
-- Etc.


type family SType t coll where
  SType t () = t
  SType t (f ()) = f t

sshow :: (Show a) => Super a coll -> Super String coll
sshow = smap show

smap :: (t -> o) -> Super t coll -> Super o coll
smap func (Super val (Mapper map)) = Super (map func val) (Mapper map)

--sjoin :: (Monad m) => Super (m o) coll -> m (Super o coll)

data Mapper coll = Mapper (forall a t. (a -> t) -> SType a coll -> SType t coll)
data Super a coll = Super (SType a coll) (Mapper coll)

instance (Show (SType a coll)) => Show (Super a coll) where
  show (Super val _) = show val


-- this function does the same thing as putStrLn
-- it can take whatever arguments you throw at it, and returns whatever it needs to

sputStrLn :: (CanBe String (IO ()) oe ie) => ie -> oe
sputStrLn = as putStrLn



-- The CanBe type class allows certain types to expose themselves to functions as other types, by taking control over how the function is executed

-- input is the type that the function expects
-- output is the type that the function returns
-- inputEmulator is the type that now exposes itself as the input type
-- outputEmulator is the type that is returned instead of output, whenever inputEmulator emulates input


-- I don't know what order to put the arguments in exactly
class CanBe input output outputEmulator inputEmulator where
  as :: (input -> output) -> inputEmulator -> outputEmulator


-- any type can trivially emulate itself
instance CanBe a c c a where
  as func = func


-- a functor of a type can emulate that type by fmapping over itself
instance (Functor f) => CanBe a c (f c) (f a) where
  as = fmap


-- a monadic value can emulate the value it contains when passed to a function that it could be bound to
instance (Monad m) => CanBe a (m c) (m c) (m a) where
  as func arg = arg >>= func


-- a function that returns a monadic value can operate on a collection of input values
-- however, it must make sure to return a monad of the traversable, not a traversable of monads
instance (Traversable t, Monad m) => CanBe a (m c) (m (t c)) (t a) where
  as = mapM
