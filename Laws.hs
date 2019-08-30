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

--!! Traversable Laws
--** Naturality
-- t . traverse f = traverse (t . f)
    -- This law tells us that function composition behaves in
    -- unsurprising ways with respect to a traversed function.
    -- Since a traversed function 𝑓 is generating the structure
    -- that appears on the “outside” of the traverse operation,
    -- there’s no reason we shouldn’t be able to float a function
    -- over the structure into the traversal itself.
--** Identity
-- traverse Identity = Identity
    -- This law says that traversing the data constructor of the
    -- Identity type over a value will produce the same result
    -- as just putting the value in Identity. This tells us Identity
    -- represents a structural identity for traversing data. This is
    -- another way of saying that a Traversable instance cannot
    -- add or inject any structure or effects.
--** Composition
-- traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
    -- This law demonstrates how we can collapse sequential
    -- traversals into a single traversal, by taking advantage of
    -- the Compose datatype, which combines structure.

--!! sequenceA Laws
--** Naturality
-- t . sequenceA = sequenceA . fmap t
--** Identity
-- sequenceA . fmap Identity = Identity
--** Composition
-- sequenceA . fmap Compose =
    -- Compose . fmap sequenceA . sequenceA