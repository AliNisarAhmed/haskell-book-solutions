module MonadLaws where

-- Identity Laws

-- m >>= return = m
-- return x >>= f = f x

-- Associativity Law

-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)