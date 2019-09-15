Beginner here:

I am having trouble writing `Monad` instances for the type:

```haskell

data Big a b = Big a b b deriving (Eq, Show)

```

The `Functor` and `Applicative` instances are straight forward (I am not absolutely sure about the `Applicative` though), but for `Monad` I am confused because of the double `b` in the Data Constructor.

Here's what I have written so far:

```haskell
instance Functor (Big a) where
    fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance (Monoid a) => Applicative (Big a) where
    pure x = Big mempty x x
    (<*>) (Big a1 f1 f2) (Big a2 x1 x2) =
        Big (a1 <> a2) (f1 x1) (f2 x2)

instance (Monoid a) => Monad (Big a) where
    return = pure
    (Big a1 b1 b2) >>= f = Big (a1 <> a2) b3 b5
        where
            Big a1 b3 b4 = f b1  -- no idea what I am doing here
            Big a3 b5 b6 = f b2  -- same as above
```

But `f` is not a binary function, and nor can I combine `b1` and `b2` with a `monoid`. So, I try to apply `f` separately to `b1` and `b2` and combine them in some way.

I'm using the checkers library to test these instances, and "left Identity" & "ap" laws of monad are failing. All applicative laws are passing.

So, my questions are:

1. Is there any particular name of the Types (such as `Big` above) where a type variable is witnessed multiple times in the Data Constructors?
2. Is it even possible to derive a `Monad` instance for this data type.

My reasoning so far has been:
The type of `>>=` is `Monad m => m a -> (a -> m b) -> m b`. More importantly, the type of `f` from the definition of `>>=` is `a -> m b`. Hence, `f` cannot take two arguments, and since `a` is not a `Monoid` , we can't combine two instances of `a`, just like `b1` and `b2` above, so Monad instance is not possible for this type.

I am more interested in the correct way of thinking about approaching this problem rather than a direct solution.
