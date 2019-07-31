module Laws where

--!! Functor Laws **--

--** Identity
-- fmap id x = x

--** Compose
-- fmap (f . g) x = fmap f . fmap g $ x


--!! Applicative Laws **--

--** Identity
-- pure id <*> x = x

--** Compose
-- composing two lifted functions u and v, applying to w
-- pure (.) <*> u <*> v <*> w
-- OR pure (.) <*> pure f <*> pure g <*> w
-- is equal to
-- applying two lifted functions to w
-- u <*> v <*> w
-- OR pure f <*> (pure g <*> w)

--** Homomorphism
-- pure f <*> pure x = pure (f x)

--** Interchange
-- u <*> pure y = pure ($ y) <*> u
-- usually <*> means -> function <*> value
-- as per this law
-- LEFT = lifted function <*> unlifted value
-- RIGHT = (value partially applied, waiting for a function) <*> (lifted function)

--!! Monad Laws

--** Identity
-- m >>= return = m => where m is a monad
-- return x >>= f = f x

--** Associativity
-- (m >> f) >> g == m >> (\x -> f x >>= g)
-- where x is the value enclosed in the monad m
