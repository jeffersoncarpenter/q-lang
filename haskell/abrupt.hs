{-# LANGUAGE TypeSynonymInstances, KindSignatures, GADTs, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

import Control.Monad


main = do
  putStrLn "hello world"                                 -- 
  ijoin $ fmap putStrLn ["hello", "world"]               -- prints "hello" and then "world"
  ijoin $ fmap putStrLn []                               -- no-op
  ijoin $ fmap putStrLn (Just "foo bar")                 -- prints "foo bar"
  ijoin $ fmap putStrLn Nothing                          -- prints an empty line
  ijoin $ fmap (ijoin . (fmap putStrLn)) [Just "hello"]  -- supports nesting types using fmap and ijoin additional times
  ijoin $ fmap putStrLn $                                
          fmap show $                                    -- supports composing functions using fmap
          fmap length ["aoeu", "aoeuaoeu"]               -- none of these functions takes an array... until now
  putStrLn "enter a string to be repeated back:"
  ijoin $ fmap putStrLn getLine



class Joinable m f where
  ijoin :: f (m a) -> m (f a)

instance (Monad m) => Joinable m [] where
  ijoin = sequence

instance (Monad m) => Joinable m Maybe where
  ijoin Nothing = return Nothing
  ijoin (Just val) = val >>= \a -> return (Just a)

instance (Monad m) => Joinable m m where
  ijoin arg = liftM return $ join arg


