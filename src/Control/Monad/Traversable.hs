module Control.Monad.Traversable
-- (>>=),  (=<<)
  ((>>-=), (=-<<)
  ,(>>=-), (-=<<)
  ,(->==), (==<-)
  ,(>>==), (==<<)

  ,(>-#=), (=#-<)
  ,(>#-=), (=-#<)
  ,(>#=-), (-=#<)
  ,(->#=), (=#<-)

  ,(>>#=), (=#<<)
  ,(>#==), (==#<)
  ,(>##=), (=##<)

  ,Monad(..)
  ) where

import Control.Monad
import Data.Functor.Compose

--------------------------------------------------------------------------------
-- Group 1
-- Outer layer is Monadic


{-
(>--=) :: (Functor m) => m a -> (a -> t b) -> m (t b)
this is the same as (<&>)

(->=-) :: (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
this is the same as `for`

(>-==) :: (Monad m, Traversable t) => m a -> (a -> m (t b)) -> m (t b)
this is the same as (>>=)
-}

infixl 2 >>-=
infixr 2 =-<<
(>>-=) :: (Functor m, Monad t) => m (t a) -> (a -> t b) -> m (t b)
(>>-=) = bindI
(=-<<) f m = m >>-= f

infixl 2 >>=-
infixr 2 -=<<
(>>=-) :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
(>>=-) = bindM
(-=<<) f m = m >>=- f

infixl 2 ->==
infixr 2 ==<-
(->==) :: (Applicative m, Monad t, Traversable t) => t a -> (a -> m (t b)) -> m (t b)
(->==) = bindIM
(==<-) f m = m ->== f

infixl 2 >>==
infixr 2 ==<<
(>>==) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)
(>>==) = bind2
(==<<) f m = m >>== f




--------------------------------------------------------------------------------
-- Group 2, Variations on Group 1
-- Pre-Sequence Function and/or Value


infixl 2 >-#=
infixr 2 =#-<
(>-#=) :: (Monad m, Traversable t) => m a -> (a -> t (m b)) -> m (t b)
(>-#=) = bindF
(=#-<) f m = m >-#= f

infixl 2 >#-=
infixr 2 =-#<
(>#-=) :: (Applicative m, Monad t, Traversable t) => t (m a) -> (a -> t b) -> m (t b)
(>#-=) = bindIV
(=-#<) f m = m >#-= f

infixl 2 >#=-
infixr 2 -=#<
(>#=-) :: (Monad m, Traversable t) => t (m a) -> (a -> m b) -> m (t b)
(>#=-) = bindMV
(-=#<) f m = m >#=- f

infixl 2 ->#=
infixr 2 =#<-
(->#=) :: (Applicative m, Monad t, Traversable t) => t a -> (a -> t (m b)) -> m (t b)
(->#=) = bindIMF
(=#<-) f m = m ->#= f




infixl 2 >>#=
infixl 2 =#<<
(>>#=) :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> t (m b)) -> m (t b)
(>>#=) = bind2F
(=#<<) f m = m >>#= f

infixl 2 >#==
infixl 2 ==#<
(>#==) :: (Monad m, Monad t, Traversable t) => t (m a) -> (a -> m (t b)) -> m (t b)
(>#==) = bind2V
(==#<) f m = m >>#= f

infixl 2 >##=
infixl 2 =##<
(>##=) :: (Monad m, Monad t, Traversable t) => t (m a) -> (a -> t (m b)) -> m (t b)
(>##=) = bind2FV
(=##<) f m = m >##= f




--------------------------------------------------------------------------------
-- The Compose Monad


instance (Monad m, Monad t, Traversable t) => Monad (Compose m t) where
  return a = Compose $ return $ return a
  m >>= f = Compose $ getCompose m >>== getCompose . f



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-

(>>=) m f = join $ fmap f m
traverse f m = sequenceA $ fmap f m

bind2FV is the "fullest" of all these functions, meaning it uses fmap, join,
and sequenceA at every possible step, 3, 2 and 3 times respectively.

Consider how the function bind works. It can simply be described as join after
fmap. Without join, the result of fmap makes the type m (m a). Without join
or sequenceA, bind2FV makes the type t (m (t (m a))). In order to create the
type m (t a), both m and t layers need to be joined respectively but to do
that, we need to move the layers around. The first 2 uses of sequenceA happen
before fmap or join are used, which is where the suffixes F and V come in.

F refers to the uses of sequenceA on the result of a function of type
(a -> t (m a)). This switches the right most t and m layers.
  - bindF
  - bindIMF
  - bind2F
  - bind2FV

V refers to the uses of sequenceA on a value of type t (m a). This
switches the left most t and m layers.
  - bindIV
  - bindMV
  - bind2V
  - bind2FV

This leaves us with 1 use of sequnceA, 2 joins, and the type
m (t (m (t a))). Lastly we switch the middle t and m layers so we can
simply use join on each sides.

While bind and traverse only work on 2 layers each and bind2, bind2FV, etc use
4 layers, there are functions "in between" that use 3 layers. In these cases,
only 1 functor is monadic. These funcitons use the suffixes I and M and can be
combined with F and V.

I refers to using t (the Inner functor) as a monad. Otherwise, the outer functor
m is monadic.
  - bindI
  - bindIM

M refers to using the the 3rd sequnceA to switch the middle t and m layers. This
is really only relevant to bindI which is just fmap after bind which is trivial.
  - bindM
  - bindIM

-}


bind   ::  Monad m                                => m    a  -> (a -> m    b ) -> m    b
bindI  :: (Functor m,     Monad t)                => m (t a) -> (a ->    t b ) -> m (t b)
bindM  :: (Monad m,                Traversable t) => m (t a) -> (a -> m    b ) -> m (t b)
bindIM :: (Applicative m, Monad t, Traversable t) =>    t a  -> (a -> m (t b)) -> m (t b)
bind2  :: (Monad m,       Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)

bind   m f = join $      (                        fmap f) $ m -- >>= / >$>=
bindI  m f =        fmap (     join .             fmap f) $ m -- >>-=
bindM  m f = join $ fmap (            sequenceA . fmap f) $ m -- >>=-
bindIM m f =             (fmap join . sequenceA . fmap f) $ m -- ->==
bind2  m f = join $ fmap (fmap join . sequenceA . fmap f) $ m -- >>==




bindF   :: (Monad m,                Traversable t) => m    a  -> (a -> t (m b)) -> m (t b)
bindIV  :: (Applicative m, Monad t, Traversable t) => t (m a) -> (a ->    t b ) -> m (t b)
bindMV  :: (Monad m,                Traversable t) => t (m a) -> (a -> m    b ) -> m (t b)
bindIMF :: (Applicative m, Monad t, Traversable t) =>    t a  -> (a -> t (m b)) -> m (t b)

bind2F  :: (Monad m,       Monad t, Traversable t) => m (t a) -> (a -> t (m b)) -> m (t b)
bind2V  :: (Monad m,       Monad t, Traversable t) => t (m a) -> (a -> m (t b)) -> m (t b)
bind2FV :: (Monad m,       Monad t, Traversable t) => t (m a) -> (a -> t (m b)) -> m (t b)

seqFuncA bind m f = bind m (sequenceA . f)
seqValA  bind m f = bind (sequenceA m) f

bindF   = seqFuncA           bind   -- >-#=
bindIV  = seqValA            bindI  -- >#-=
bindMV  = seqValA            bindM  -- >#=-
bindIMF = seqFuncA           bindIM -- ->#=

bind2F  = seqFuncA           bind2  -- >>#=
bind2V  = seqValA            bind2  -- >#==
bind2FV = seqValA $ seqFuncA bind2  -- >##=



{-
bind    m f = join $      (                        fmap              f )             $ m :: m    a  -> (a -> m    b ) -> m    b
bindF   m f = join $      (                        fmap (sequenceA . f))             $ m :: m    a  -> (a -> t (m b)) -> m (t b)
bindI   m f =        fmap (     join .             fmap              f )             $ m :: m (t a) -> (a ->    t b ) -> m (t b)
bindIV  m f =        fmap (     join .             fmap              f ) . sequenceA $ m :: t (m a) -> (a ->    t b ) -> m (t b)
bindM   m f = join $ fmap (            sequenceA . fmap              f )             $ m :: m (t a) -> (a -> m    b ) -> m (t b)
bindMV  m f = join $ fmap (            sequenceA . fmap              f ) . sequenceA $ m :: t (m a) -> (a -> m    b ) -> m (t b)
bindIM  m f =             (fmap join . sequenceA . fmap              f )             $ m ::    t a  -> (a -> m (t b)) -> m (t b)
bindIMF m f =             (fmap join . sequenceA . fmap (sequenceA . f))             $ m ::    t a  -> (a -> t (m b)) -> m (t b)
bind2   m f = join $ fmap (fmap join . sequenceA . fmap              f )             $ m :: m (t a) -> (a -> m (t b)) -> m (t b)
bind2F  m f = join $ fmap (fmap join . sequenceA . fmap (sequenceA . f))             $ m :: m (t a) -> (a -> t (m b)) -> m (t b)
bind2V  m f = join $ fmap (fmap join . sequenceA . fmap              f ) . sequenceA $ m :: t (m a) -> (a -> m (t b)) -> m (t b)
bind2FV m f = join $ fmap (fmap join . sequenceA . fmap (sequenceA . f)) . sequenceA $ m :: t (m a) -> (a -> t (m b)) -> m (t b)
-}
