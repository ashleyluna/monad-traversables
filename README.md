# Monad-Traversables

### `>>== :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)`

Ok, so I'll make this simple.

Boilerplate code is annoying. There isn't a lot of it in Haskell since the syntax is very light (very few keywords, little use of brackets, and lots of whitespace), but when things get complex, certain patterns arise and the process of managing code can start to become exhausting when long blocks of code get copy pasted with tiny variations in variable names.

I started running into patterns with 2 Functor layers when I was writing IO code that could fail for one reason or another (http libraries defaulting to return types of `IO (Either a b)` or database requests that always returned `IO (Maybe a)` or `IO [a]`). Here are some trivial examples with different ways of reducing boilerplate code.

Case analysis and Do notation
```Haskell
ioMaybeValue :: IO (Maybe a)
ioFunction   :: a -> IO b
maybeValue   :: Maybe a
ioMaybefunction :: a -> IO (Maybe b)

example1 :: IO (Maybe b)
example1 = do
  maybeValue <- ioMaybeValue
  case maybeValue of
       Just value -> fmap Just $ ioFunction value
       Nothing -> return Nothing

example2 :: IO (Maybe b)
example2 = case maybeValue of
  Just value -> ioMaybefunction value
  Nothing -> return Nothing
```

Bind and Traverse
```Haskell
example1 = ioMaybeValue >>= traverse ioFunction
example2 = fmap join $ traverse ioMaybefunction maybeValue
```

Monad Transformers
```Haskell
example1 = runMaybeT $ do
  value <- MaybeT ioMaybeValue
  lift $ ioFunction value
example2 = runMaybeT $ do
  value <- hoistMaybe maybeValue
  MaybeT $ ioMaybefunction value
```

So obviously just using case analysis and do notation is very BLEH. WAAAY too much unnecessary code. Lots of variable names that are only being created to pass into another functions 1 time. Using `>>=` and `traverse` makes the visually more appealing but `ioMaybeValue >>= traverse ioFunction` doesn't convey that much information, mostly because `>>=` and `traverse` are already highly abstracted and trying to juggle what happens to the `Maybe` layer with `fmap join` in `fmap join $ traverse ioMaybefunction maybeValue` will probably confuse the person who wrote that line a week later when they have to go back to that line and update the code. Overall it's not a great solution.

The more obvious solution is to just use Monad Transformers. The only juggling of Functor layers involved is getting `a -> IO b` and `Maybe a` to return `MaybeT IO b` values because `MaybeT IO` only counts as 1 Functor layer. However, the actual use of Monad Transformers in this method is completely redundant. `MaybeT IO` when combined with `>>=` *should* be used to bind 2 `IO` layers and 2 `Maybe` layers. Instead either 1 "empty" `IO` or `Maybe` layer is created with `lift` or `hoistMaybe` and then the return value is converted back to `Maybe (IO b)` so the *Monad Trnasformer* part of this code isn't actually being used for it's actual purpose. It's like writing `return value >>= ioFunction`, `return` is just being used to make the types line up for `>>=` even tho `IO` isn't actually being used "as a Monad". Not only that, but the name `value` gets created just to be passed into another function on the next line. Monad Transformers work here but they don't "do" what they're supposed to and what they *are* doing is almost identical to case analysis. It's not actually removing that much boilerplate code, it's really just moving it around.

Monad Traversables
```Haskell
(>>=-) :: (Monad m, Traversable t)                => m (t a) -> (a -> m    b ) -> m (t b)
(->==) :: (Applicative m, Monad t, Traversable t) =>    t a  -> (a -> m (t b)) -> m (t b)

example1 = ioMaybeValue >>=- ioFunction
example2 = maybeValue ->== ioMaybefunction
```

Imo, not only is this readable (once you learn the naming scheme) and convey information about how these values are being used so you don't have to juggle all the Functor layers in your head, but you dont have to create unnecessary variable names which leads to *actually* removing boilerplate code.

---

```
(<&>)  :: (Functor m)                             => m    a  -> (a ->    t b ) -> m (t b)
for    :: (Applicative m,          Traversable t) =>    t a  -> (a -> m    b ) -> m (t b)
(>>=)  :: (Monad m,                Traversable t) => m    a  -> (a -> m (t b)) -> m (t b)
```
```
(->==) :: (Applicative m, Monad t, Traversable t) =>    t a  -> (a -> m (t b)) -> m (t b)
(>>-=) :: (Functor m,     Monad t               ) => m (t a) -> (a ->    t b ) -> m (t b)
(>>=-) :: (Monad m,                Traversable t) => m (t a) -> (a -> m    b ) -> m (t b)
(>>==) :: (Monad m,       Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)
```
```
(->#=) :: (Applicative m, Monad t, Traversable t) => t    a  -> (a -> t (m b)) -> m (t b)
(>-#=) :: (Monad m,                Traversable t) =>    m a  -> (a -> t (m b)) -> m (t b)
(>#-=) :: (Applicative m, Monad t, Traversable t) => t (m a) -> (a -> t    b ) -> m (t b)
(>#=-) :: (Monad m,                Traversable t) => t (m a) -> (a ->    m b ) -> m (t b)

(>#==) :: (Monad m,       Monad t, Traversable t) => t (m a) -> (a -> m (t b)) -> m (t b)
(>>#=) :: (Monad m,       Monad t, Traversable t) => m (t a) -> (a -> t (m b)) -> m (t b)
(>##=) :: (Monad m,       Monad t, Traversable t) => t (m a) -> (a -> t (m b)) -> m (t b)
```

If it wasn't obvious these are based on the function `>>=`. The easiest to understand it `>>==`.

`>>== :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)`

It maps over Monadic layers like `>>=` does except there's 2 and the inner layer is `Traversable`, easy right?

The other functions are just mix and match for each layer. If the `Traversable` layer is outside the other Monadic layer, then the function name will have a `#`, so `>##=` has `t (m a)` instead of `m (t a)` for both the input value and function and the placement of the `#` corresponds to whether the value and/or the function has the Monadic layers flipped.

A `-` means there's a "hole" in the type signature.

`(->==) :: (Applicative m, Monad t, Traversable t) => t a -> (a -> m (t b)) -> m (t b)`

For example, the value on the left side is `t a` instead of `m (t a)` so the `m` layer is "missing". There are 4 unique layers (2 for the left value and 2 for the output of the right function) and 4 characters in each operator name so the `-` is placed from left to right depending on the missing layer. 2 `-` signs would correspond to `>>=`, `traverse`, or just `fmap` so there's only 1 per operator.

And that's it, the name of each operator is entirely based on their type signatures, and the character replacements don't overlap because a value can't have a "missing layer" *and* have those layers flipped at the same time.

So for instance, `>#=-` means that the left value is `t (m a)` because the `#` is on the left half and the right function is `a -> m b` because the `-` is on the far right side.

At the bottom of the file `Control.Monad.Traversable`, there's a more in-depth explanation about the patterns of each function. It's quite a lot tho lol.

---

Some real life examples include (`>>=-`) taking data from a database with a return type of `IO (Maybe a)` (this is common when using `persistent`) and needing to some IO with whatever data was retrieved `a -> IO b`. This can include adding this information to memory so we don't have to ask the database over and over again for user profile information if they are connected to the server via websockets or simply sending some of that information to the user's computer. Another situation (`->==`)  is needing to retrieve some data from the database *if* a user "owns" an id value (`Maybe a` and `a -> IO (Maybe b)`). The database will retrieve the value if it exists, otherwise `Just Nothing` simplifies to `Nothing` and we return `IO (Maybe b)` no matter what so we don't have to worry about creating unnecessary varaible names with case analysis.

```Haskell
case maybeUserGoogleAuth of
     Nothing -> return Nothing
     Just userGoogleAuth -> do
       maybeDBGoogleAuth <- runDB $ get userGoogleAuth
       case maybeDBGoogleAuth of
         Nothing -> return Nothing
         Just DB.GoogleAuth{..} -> fmap Just $ sendToUser $ toJSON emailAddress
```
```Haskell
do userGoogleAuth <- hoistMaybe maybeUserGoogleAuth
   DB.GoogleAuth{..} <- MaybeT $ runDB $ get userGoogleAuth
   lift $ sendToUser $ toJSON emailAddress
```
```Haskell
maybeUserGoogleAuth ->== runDB . get >>=- \DB.GoogleAuth{..} ->
  sendToUser $ toJSON emailAddress
```
