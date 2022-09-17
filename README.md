# Monad-Traversables

Ok, so I'll make this simple.

The reason why Monad Transformers are popular instead of a generalized function that operates over 2 Monadic Layers (i.e. `m1 (m2 a) -> (a -> m1 (m2 b))`) is because in this case `m2` has to be `Traversable` which means that this function can't be used on `Reader e (IO a)` because neither `Reader` or `IO` are `Traversable`. Monad Transformers like `ReaderT` solve this by manually defining the output to have a Functor as a container.

However, it doesn't mean both can't exist at the same time. Personally, I've ran into `IO (Maybe a)` (waaaay too often to count) where I need to use case analysis over the `Maybe` type because I'm using `Maybe` to act as a `Functor` rather than as a `Monad` so `MaybeT` isn't useful in those situations. I also often have to map an `IO` function over an `IO [a]`, imo it's kinda annoying to have to use an unnecessary `do` block for `ListT` for an operation that's equivalent to using `bind` and `traverse`.

In these cases, `Maybe` and `List` are both `Traversable` so I've just found it way easier to use these functions I made. (I promise it's easier to understand than it looks at a first glance)

```
(>>-=) :: (Functor m, Monad t) => m (t a) -> (a -> t b) -> m (t b)
(>>=-) :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
(->==) :: (Applicative m, Monad t, Traversable t) => t a -> (a -> m (t b)) -> m (t b)
(>>==) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)

```
```
(>-#=) :: (Monad m, Traversable t) => m a -> (a -> t (m b)) -> m (t b)
(>#-=) :: (Applicative m, Monad t, Traversable t) => t (m a) -> (a -> t b) -> m (t b)
(>#=-) :: (Monad m, Traversable t) => t (m a) -> (a -> m b) -> m (t b)
(->#=) :: (Applicative m, Monad t, Traversable t) => t a -> (a -> t (m b)) -> m (t b)

(>>#=) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> t (m b)) -> m (t b)
(>#==) :: (Monad m, Monad t, Traversable t) => t (m a) -> (a -> m (t b)) -> m (t b)
(>##=) :: (Monad m, Monad t, Traversable t) => t (m a) -> (a -> t (m b)) -> m (t b)
```

If it wasn't obvious these are based on the function `>>=`. The easiest to understand it `>>==`.

`>>== :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)`

It maps over Monadic layers like `>>=` does except there's 2 and the inner layer is `Traversable`, easy right?

The other functions are just mix and match for each layer. If the `Traversable` layer is outside the other Monadic layer, then the function name will have a `#`, so `>##=` has `t (m a)` instead of `m (t a)` for both the input value and function and the placement of the `#` corresponds to whether the value and/or the function has the Monadic layers flipped.

A `-` means there's a "hole" in the type signature.

`(->==) :: (Applicative m, Monad t, Traversable t) => t a -> (a -> m (t b)) -> m (t b)`

For example, the value on the left side is `t a` instead of `m (t a)` so the `m` layer is "missing". There are 4 unique layers (2 for the value and 2 for the output of the function) and 4 characters in each operator name so the `-` is placed from left 2 right depending on the missing layer. 2 `-` signs would correspond to `>>=`, `traverse`, or just `fmap` so there's only  1 per operator.

And that's it, the name of each operator is entirely based on their type signatures, and the character replacements don't overlap because a value can't have a "missing layer" *and* have those layers flipped at the same time.

At the bottom of the file `Control.Monad.Traversable`, there's a more in-depth explanation about the patterns of each function. It's quite tho lol.
