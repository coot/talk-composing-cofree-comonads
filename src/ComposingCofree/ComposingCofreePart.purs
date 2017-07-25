module ComposingCofree.ComposingCofreePart where

import Prelude (discard, ($))
import Text.Smolder.HTML hiding (style)
import Text.Smolder.HTML.Attributes (className, href, target)
import Text.Smolder.Markup (Markup, text, (!))

composingCofreePart :: forall e. Markup e
composingCofreePart = do
  section $ do
    h1 $ text "Composing Cofree interpreters"
    h2 $ text "part ii"
  section $ do
    h1 $ text "Free monad"
    h3 $ text "definition"
    pre $ code ! className "language-haskell" $ text """data Free f a
  = Pure a 
  | Free f (Free f a)

  derive instance functorFree :: Functor (Free f)

  instance bindFree :: Bind (Free f) where
  bind (Pure a) f = f a
  bind (Free m) f = (Free (f <$> m))

  instance applyFree :: Apply (Free f) where
  apply = ap

  instance applicativeFree :: Applicative (Free f) where
  pure a = Pure a

  instance monadFree :: Monad (Free f)"""
  section $ do
    h1 $ text "Comonad"
    h3 $ text "definition"
    pre $ code ! className "language-haskell" $ text """-- | Associativity: `extend f <<< extend g = extend (f <<< extend g)`
  class Functor w <= Extend w where"""

    pre $ code ! className "language-haskell" $ text """-- | Forwards co-Kleisli composition.
  composeCoKleisli
  :: forall b a w c
  . Extend w
  => (w a -> b)
  -> (w b -> c)
  -> w a -> c
  composeCoKleisli f g w = g (f <<= w)

  -- | Duplicate a comonadic context.
  duplicate
  :: forall a w
  . Extend w
  => w a -> w (w a)
  duplicate = extend id"""
    pre $ code ! className "language-haskell" $ text """-- | Left Identity: `extract <<= xs = xs`
  -- | Right Identity: `extract (f <<= xs) = f xs`
  class Extend w <= Comonad w where
  extract :: forall a. w a -> a"""
  section $ do
    h1 $ text "Stream Comonad"
    pre $ code ! className "language-haskell" $ text """data Stream a = Cons a (Lazy (Stream a))

  derive instance functorStream :: Functor Stream

  instance extendStream :: Extend Stream where
  extend f s@(Cons a r) = Cons (f s) (extend f <$> r)

  instance comonadStream :: Comonad Stream where
  extract (Cons a _) = a

  smooth :: Int -> Stream Number -> Stream Number
  smooth n s = extend (avN n) s
  where
  sumN :: Int -> Stream Number -> Number
  sumN 0 _ = 0.0
  sumN n (Cons a r) = a + (sumN (n - 1) (force r))

  avN :: Int -> Stream Number -> Number
  avN n s = (sumN n s) / (toNumber n)"""
  section $ do
    h1 $ text "Cofree comonad"
    text "Given a functor `f` we can build a cofree comonad"
    pre $ code ! className "language-haskell" $ text """
  data Cofree f a = Cofree a (Lazy (f (Cofree f a)))

  mkCofree :: forall f a. a -> f (Cofree f a) -> Cofree f a
  mkCofree a t = Cofree a (defer \_ -> t)

  infixr 5 mkCofree as :<

  -- | Returns the label for a tree.
  head :: forall f a. Cofree f a -> a
  head (Cofree h _) = h

  instance functorCofree :: Functor f => Functor (Cofree f) where
  map f = loop where
  loop fa@(Cofree head _tail)
  = Cofree (f head) ((map loop) <$> _tail)

  instance extendCofree :: Functor f => Extend (Cofree f) where
  extend f = loop
  where
  loop fa@(Cofree _ _tail)
  = Cofree (f fa) ((map loop) <$> _tail)

  instance comonadCofree :: Functor f => Comonad (Cofree f) where
  extract = head"""
  section $ do
    h1 $ text "Composing interpreters"
    h4 $ a ! className "center" ! href "http://try.purescript.org/?gist=b31f48d16ad43cec8c0afcd470ac5add" ! target "_blank" $ text "live demo"
  section $ do
    pre $ code ! className "language-haskell" $ text """unfoldCofree
  :: forall f s a
  . Functor f
  => (s -> a)
  -> (s -> f s)
  -> s
  -> Cofree f a
  unfoldCofree e n s =
  Cofree (e s) (defer \_ -> unfoldCofree e n <$> n s)"""
    pre $ code ! className "language-haskell" $ text """explore
  :: forall f g a b
  . Functor f
  => Functor g
  => (forall x y. f (x -> y) -> g x -> y)
  -> Free f (a -> b)
  -> Cofree g a
  -> b
  explore pair m w =
  case runState (runFreeM step m) w of
  Tuple f cof -> f (extract cof)
  where
  step
  :: f (Free f (a -> b))
  -> State (Cofree g a) (Free f (a -> b))
  step ff = state \cof -> pair (map Tuple ff) (tail cof)"""
  section $ do
    pre $ code ! className "language-haskell" $ text """newtype Coproduct f g a = Coproduct (Either (f a) (g a))

  -- | Left injection
  left :: forall f g a. f a -> Coproduct f g a
  left fa = Coproduct (Left fa)

  -- | Right injection
  right :: forall f g a. g a -> Coproduct f g a
  right ga = Coproduct (Right ga)"""
    pre $ code ! className "language-haskell" $ text """newtype Product f g a = Product (Tuple (f a) (g a))

  -- | Create a product.
  product :: forall f g a. f a -> g a -> Product f g a
  product fa ga = Product (Tuple fa ga)"""
  section $ do
    pre $ code ! className "language-haskell" $ text """-- | Returns the label for a tree.
  head :: forall f a. Cofree f a -> a
  head (Cofree h _) = h

  -- | Returns the "subtrees" of a tree.
  tail :: forall f a. Cofree f a -> f (Cofree f a)
  tail (Cofree _ t) = force t

  mkCofree :: forall f a. a -> f (Cofree f a) -> Cofree f a
  mkCofree a t = Cofree a (defer \_ -> t)"""
  section $ do
    h1 $ text "Composing Streams"
    p ! className "center" $ do
      a ! href "http://try.purescript.org/?gist=6bac5a9839403f65c40e8494031c9a00" ! target "_blank" $ text "live demo"
