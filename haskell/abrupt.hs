{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, UndecidableInstances, FlexibleContexts, FunctionalDependencies, IncoherentInstances #-}

import Control.Monad



main = do
  -- prints a string
  sputStrLn "aoeu"

  -- prints an array of strings
  sputStrLn ["hello", "world"]

  -- accepts an empty array (but you have to explicitly type it - eww)
  sputStrLn ([] :: [String])

  -- prints a nullable string
  sputStrLn (Just "foo bar")

  -- prints nothing if the nullable string is null (explicitly typed eww gross)
  sputStrLn (Nothing :: Maybe String)

  -- the trick happens twice when you pass in an array of nullable types
  sputStrLn [Just "hello"]

  -- sshow and slength don't work yet
  sputStrLn $ fmap show $ fmap length ["aoeu", "aoeuaoeu"]

  -- binds monads appropriately
  sputStrLn "enter a string to be repeated back:"
  sputStrLn getLine



-- this main function looks much cleaner than before, but it still has some problems:


-- 1: "empty" functors (like [] and Nothing) need to be explicitly typed

-- 2: sshow doesn't compile
-- I think this is because show could have separate implementations for [t] and t
-- CanBe's as function wouldn't know whether to use the [t] implementation or recursively apply the t implementation


data Super coll t a = Super (coll, t)


data Atom

class TypeCast   a b   | a -> b, b -> a     where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t -> a -> b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t -> a -> b

instance TypeCast' () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x = x



type family ShowType t where
  

class Showable s t | t -> s where
  toShow :: t -> s

instance (TypeCast a b) => Showable a b where
  toShow = id


class IsCollection a t coll | a t -> coll, a coll -> t, t coll -> a where
  type Apply a t coll o :: *
  imap :: (IsCollection o (Apply a t coll o) coll) => (a -> o) -> t -> (Apply a t coll o)
  
instance IsCollection a [a] [()] where
  type Apply a [a] [()] o = [o]
  imap = fmap


--instance (TypeCast a t, TypeCast Atom coll) => IsCollection a t coll





class CB a t coll where
  cmap :: (CB o r coll) => (a -> o) -> t -> r




class (r ~ CBResult t e o, t ~ CBInput e o r) => CanBe'' t e o r | t e o -> r where
  as'' :: (t -> o) -> e -> r


class CanBe' coll t e o r | coll t e o -> r where
  as' :: coll -> (t -> o) -> e -> r

instance CanBe' Atom t t o o where
  as' _ = id

instance (Functor f, CanBe'' t e o r) => CanBe' (f ()) t (f e) o (f r) where
  as' _ = fmap . as''


type family CBResult t e o where
  CBResult t t o = o
  CBResult t (f e) o = f (CBResult t e o)

type family CBInput e o r where
  CBInput e o o = e
  CBInput (f e) o (f r) = CBInput e o r

--slength :: (Show a, Is
--slength arg = as length arg


--sshow :: (Show a, CanBe a e String) => e -> CBResult a e String
--sshow arg = cmap show arg


sputStrLn :: (CanBeM IO String e ()) => e -> IO (CBResult String e ())
sputStrLn = asM putStrLn


class (t ~ CBInput e o (CBResult t e o)) => CanBeM m t e o where
  asM :: (t -> m o) -> e -> m (CBResult t e o)

instance CanBeM m t t o where
  asM func = func

instance (Functor f,
          Joinable m f,
          f o ~ CBResult t (f t) o,
          t ~ CBInput (f t) o (f o)) => CanBeM m t (f t) o where
  asM func = ijoin . fmap func

instance (Functor f,
          Joinable m f,
          f o ~ CBResult t (f t) o,
          t ~ CBInput (f t) o (f o),
          Functor g,
          Joinable m g,
          g (f o) ~ CBResult t (g (f t)) o,
          t ~ CBInput (g (f t)) o (g (f o))) => CanBeM m t (g (f t)) o where
  asM func = ijoin . fmap (ijoin . fmap func)



class (t ~ CBInput e o (CBResult t e o)) => CanBe t e o where
  as :: (t -> o) -> e -> CBResult t e o

instance CanBe t t o where
  as func = func

instance (CBResult t (f e) o ~ f (CBResult t e o),
          t ~ CBInput (f e) o (f (CBResult t e o)),
          CanBe t e o,
          Functor f) => CanBe t (f e) o where
  as func = fmap $ as func


class Joinable m f where
  ijoin :: f (m a) -> m (f a)

instance (Monad m) => Joinable m [] where
  ijoin = sequence

instance (Monad m) => Joinable m Maybe where
  ijoin Nothing = return Nothing
  ijoin (Just val) = val >>= \a -> return (Just a)

instance (Monad m) => Joinable m m where
  ijoin arg = liftM return $ join arg


