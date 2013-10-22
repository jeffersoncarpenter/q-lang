{-# LANGUAGE TypeSynonymInstances, KindSignatures, GADTs, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

main = do
  -- ijoin $ imap putStrLn "hello world"                 -- the base case does not work!
  ijoin $ imap putStrLn ["hello", "world"]               -- prints "hello" and then "world"
  ijoin $ imap putStrLn []                               -- no-op
  ijoin $ imap putStrLn (Just "foo bar")                 -- prints "foo bar"
  ijoin $ imap putStrLn Nothing                          -- prints an empty line
  ijoin $ imap (ijoin . (imap putStrLn)) [Just "hello"]  -- supports nesting types
  ijoin $ imap putStrLn $                                
          imap show $                                    -- supports composing functions using imap
          imap length ["aoeu", "aoeuaoeu"]               -- none of these functions takes an array... until now



class Invisible f where
  imap :: (t -> u) -> f t -> f u
  ijoin :: (Monad m) => f (m t) -> m (f t)


instance Invisible [] where
  imap = map
  ijoin = sequence

instance Invisible Maybe where
  imap = fmap
  ijoin Nothing = return Nothing
  ijoin (Just val) = val >>= \a -> return (Just a)



