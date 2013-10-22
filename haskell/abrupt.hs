{-# LANGUAGE TypeSynonymInstances, KindSignatures, GADTs, TypeFamilies, MultiParamTypeClasses #-}

main = do
  -- ijoin $ imap putStrLn "hello world"   -- the base case does not work!
  ijoin $ imap putStrLn ["hello", "world"] -- prints "hello" and then "world"
  ijoin $ imap putStrLn []                 -- no-op
  ijoin $ imap putStrLn (Just "foo bar")   -- prints "foo bar"
  ijoin $ imap putStrLn Nothing            -- prints an empty line



class CanBe t e where
  type CBResult t e u :: *
  cmap :: t -> u -> e -> CBResult t e u
  join :: (Monad m) => CBResult t (m e) u -> m (CBResult t e u)


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


type IdType a = a

-- does not compile
--instance Invisible IdType where
--  imap func = func
--  ijoin = id
