{-# LANGUAGE TypeSynonymInstances, KindSignatures, GADTs, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, FlexibleContexts, OverlappingInstances #-}

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

  -- it happens twice when you pass in an array of nullable types
  sputStrLn [Just "hello"]

  -- this one doesn't work yet
  sputStrLn $ fmap show $ fmap length ["aoeu", "aoeuaoeu"]

  -- binds monads appropriately
  sputStrLn "enter a string to be repeated back:"
  sputStrLn getLine



-- this code looks much cleaner than before, but it still has some problems:


-- 1: "empty" functors (like [] and Nothing) need to be explicitly typed

-- 2: sshow doesn't compile
-- I think this is because show could have separate implementations for [t] and t
-- CanBe's as function wouldn't know whether to use the [t] implementation or recursively apply the t implementation




type family CBResult t e o where
  CBResult t t o = o
  CBResult t (f e) o = f (CBResult t e o)


sputStrLn :: (CanBeM IO String e ()) => e -> IO (CBResult String e ())
sputStrLn = asM putStrLn


class CanBeM m t e o where
  asM :: (t -> m o) -> e -> m (CBResult t e o)

instance CanBeM m t t o where
  asM func = func

instance (Functor f, Joinable m f, f o ~ CBResult t (f t) o) => CanBeM m t (f t) o where
  asM func = ijoin . fmap func

instance (Functor f,
          Joinable m f,
          f o ~ CBResult t (f t) o,
          Functor g,
          Joinable m g,
          g (f o) ~ CBResult t (g (f t)) o) => CanBeM m t (g (f t)) o where
  asM func = ijoin . fmap (ijoin . fmap func)



class CanBe t e o where
  as :: (t -> o) -> e -> CBResult t e o

instance CanBe t t o where
  as func = func

instance (CBResult t (f e) o ~ f (CBResult t e o), CanBe t e o, Functor f) => CanBe t (f e) o where
  as func = fmap (as func)


class Joinable m f where
  ijoin :: f (m a) -> m (f a)
  imap :: (a -> b) -> f a -> f b

instance (Monad m) => Joinable m [] where
  ijoin = sequence

instance (Monad m) => Joinable m Maybe where
  ijoin Nothing = return Nothing
  ijoin (Just val) = val >>= \a -> return (Just a)

instance (Monad m) => Joinable m m where
  ijoin arg = liftM return $ join arg


