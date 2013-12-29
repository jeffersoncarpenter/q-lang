{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, ExistentialQuantification, Rank2Types, UndecidableInstances #-}

import Prelude hiding (foldr, mapM, sequence)
import Control.Applicative
import Control.Monad hiding (mapM, sequence)
import Data.Traversable


data Super t = Super t
             | forall f. (Functor f, Traversable f) => SuperF (f t)

class IsA t c where
  super :: c -> Super t
  
instance IsA t t where
  super = Super
instance (Functor f, Traversable f) => IsA t (f t) where
  super = SuperF


instance Functor Super where
  fmap func (Super t) = Super (func t)
  fmap func (SuperF ft) = SuperF (fmap func ft)


sjoin :: (Applicative f) => Super (f t) -> f (Super t)
sjoin (Super at) = Super <$> at
sjoin (SuperF fat) = SuperF <$> sequenceA fat

sputStrLn :: Super String -> IO (Super ())
sputStrLn = sjoin . fmap putStrLn

sshow :: (Show a) => Super a -> Super String
sshow = fmap show

slength :: Super [a] -> Super Int
slength = fmap length

main = sputStrLn $ sshow $ slength (super ["aoeu", "aoeuhtns"] :: Super String)
