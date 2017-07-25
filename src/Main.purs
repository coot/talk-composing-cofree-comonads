module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (foldFree)
import Control.Monad.State (State, execState, state)
import Data.CatList (CatList)
import Data.Foldable (fold, foldl)
import Data.Monoid (append)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Node.Buffer (BUFFER, fromString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (writeFile)
import Prelude (type (~>), Unit, bind, discard, map, pure, ($), (/=), (<<<), (<>), (==), (>), (>>>), (||))
import Text.Smolder.HTML hiding (style)
import Text.Smolder.HTML.Attributes (className, defer, href, lang, rel, src, style, target)
import Text.Smolder.Markup (Attr(..), Markup, MarkupM(..), text, (!))

doc :: forall e. Markup e
doc = html ! lang "en" $ do
  head do
    link ! rel "stylesheet" ! href "css/reveal.css"
    link ! rel "stylesheet" ! href "css/theme/night.css"
    link ! rel "stylesheet" ! href "css/arta.css"
    link ! rel "stylesheet" ! href "css/theme/custom.css"
  body do
    div ! className "reveal" $ do
      div ! className "slides" $ do
        section do
          h1 $ text "Composing Cofree Comonads"
          h6 $ text "An introduction to PureScript"
          p ! className "center" $
            a ! target "_blank" $
              small $ text "https://purescript.org"
          p ! className "center" $ do
            text "Marcin Szamotulski, PhD"
            div ! className "center" $
              a ! target "_blank" ! href "https://github.com/coot" $
                text "github: @coot"
            div ! className "center" $
              a ! target "_blank" ! href "https://twitter.com/me_coot" $
                text "twitter: @me_coot"
	section $ do
	  h1 $ text "PureScript type system"
	  h2 $ text "part i"
        section $ do
          h2 $ text "PureScript"
          p $ text "A strongly-typed functional programming language that compiles to JavaScript"
          p $ text "Heavily inspired by Haskell."
          ul ! style "font-size: .8em" $ do
            li $ text "Type inference"
            li $ text "Records"
            li $ text "Row polymorphism"
            li $ text "Newtypes and type aliases"
            li $ text "Polymorphic types"
            li $ text "Higher-kinded types"
            li $ text "Rank N Types"
            li $ text "Multi-parameter type classes &amp; constrained types"
            li $ text "Functional dependencies"
            li $ text "Kind system with kind inference"
	section $ do
	  h1 $ text "Records"
	  pre $ code ! className "language-haskell" $ text "data Record :: # Type -> Type"
	  pre $ code ! className "language-haskell" $ text """a :: { a :: Int, b :: String }
a = { a: 1, b: "" }"""
	  pre $ code ! className "language-haskell" $ text """-- in psci
>:k { a :: Int, b :: String }
Type
>:k ( a :: Int, b :: String )
# Type"""
	section $ do
	  h1 $ text "Row polymorphism"
	  h3 $ text "extensible records"
	  pre $ code ! className "language-haskell" $ text """
add :: forall r. Int -> { a :: Int | r } - { a :: Int | r }
add x r@{ a }  = r { a = a + x}"""
          ul $ do
            li $ text "rows are association list from labels to types"
            li $ text "rows can have duplicate labels"
            li $ text "unification of rows ignores order of different labels, but preserves order of duplicates"
            li $ pre ! className "inline" $ do
              code $ text "row of k"
              span $ text " is a new kind for every kind "
              pre ! className "inline" $ code $ text "k"
	section $ do
	  h1 $ text "Row polymorphism"
	  h3 $ text "extensible effects"
	  pre $ code ! className "language-haskell" $ text """
main :: forall e. Eff
  ( console :: CONSOLE
  , http :: HTTP
  , fs :: FS
  , buffer :: BUFFER
  , avar :: AVAR
  , redox :: RedoxStore
      ( read :: ReadRedox
      , write :: WriteRedox
      , subscribe :: SubscribeRedox
      , create :: CreateRedox )
  | e )
  Unit
main = do
  log "starting..."
  store <- mkStore initialState
  runServer defaultOptionsWithLogging {} (app store)"""
	section $ do
	  h1 $ text "Ranked N Types"
	  pre $ code ! className "language-haskell" $ text """foreign import data Exists :: (Type -> Type) -> Type

mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce

runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists = unsafeCoerce"""

	section $ do
	  h1 $ text "Multi-parameter Type Classes"
	  pre $ code ! className "language-haskell" $ text """
class Monad m <= MonadState s m where
  get :: m s
  put :: s -> m Unit"""
	section $ do
	  h1 $ text "Functional Dependencies"
	  pre $ code ! className "language-haskell" $ text """
class RowToList (row :: # Type)
                (list :: RowList) |
                row -> list"""
	  p $ text "Type level prolog"
	  pre $ code ! className "language-haskell" $ text """
data Z
data S n

class Succ n m | m -> n
instance succInst :: Succ x (S x)

class Pred n m | m -> n
instance predInst :: (Succ x y) => Pred y x

class Gt x y
instance gtpp :: (Pred x xp, Pred y yp, GtEq xp yp) => GtEq x y
instance gt :: Gt (S x) Z"""
	section $ do
	  small $ do
            pre $ code ! className "language-haskell" $ text """-- | in psci
> true :: Gt (S (S Z)) (S Z)  => Boolean
true
> g :: Gt (S (S Z)) (S (S Z)) => Boolean
> g = true
Error found:
in module $PSCI
at  line 1, column 1 - line 1, column 1

  Type class instance for
		
    Main.Succ t1
	      t0
		
  is possibly infinite.

while solving type class constraint
	      
  Main.Pred t0
	    t1
	      
while inferring the type of g
in value declaration it

where t0 is an unknown type
      t1 is an unknown type

See https://github.com/purescript/documentation/blob/master/errors/PossiblyInfiniteInstance.md for more information,
or to contribute content related to this error."""
	    text "Some fancy examples:"
	    ul $ do
	      li $ do
                a ! href "https://github.com/purescript/purescript-typelevel-prelude" $ text "type level prelude:"
                small $ text "rows, symbols, type level booleans"
	      li $ do
                a ! href "https://github.com/LiamGoodacre/purescript-type-map" $ text "type level lists and maps"
	      li $ a ! href "https://github.com/LiamGoodacre/purescript-type-lang" $ text "type level lambda caluculus"
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
        script ! src "lib/js/head.min.js" $ text ""
        script ! src "lib/js/head.min.js" $ text ""
        script ! src "js/reveal.js" $ text ""
        script $ do text """
Reveal.initialize({
  controls: false,
  history: true,
  slideNumber: true,
  maxScale: 1,
  height: 900,
  dependencies: [
    {
      src: 'plugin/highlight/highlight.js',
      "async": true,
      callback: function() {
        hljs.initHighlightingOnLoad();
      }
    },
  ],
});
"""

showAttrs :: CatList Attr -> String
showAttrs = fold <<< (map showAttr)
  where
    showAttr (Attr key value) = " " <> key <> "=\"" <> value <> "\""

renderItem :: forall e. MarkupM e ~> State String
renderItem (Element name children attrs _ rest) =
  let c = render children
      b = "<" <> name <> showAttrs attrs <>
          (if length c > 0 || name == "script"
           then ">" <> c <> "</" <> name <> ">"
           else "/>")
  in state \s → Tuple rest $ append s b
renderItem (Content text rest) = state \s → Tuple rest $ append s $ text
renderItem (Empty rest) = pure rest

render :: ∀ e. Markup e → String
render f = execState (foldFree renderItem f) ""

main :: forall e. Eff (console :: CONSOLE, buffer :: BUFFER, fs :: FS, exception :: EXCEPTION | e) Unit
main = do
  buf <- fromString (render doc) UTF8
  writeFile "index.html" buf
