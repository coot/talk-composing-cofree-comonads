module ComposingCofree.PureScriptIntro where

import Prelude (discard, ($))
import Text.Smolder.HTML hiding (style)
import Text.Smolder.HTML.Attributes (className, href, style)
import Text.Smolder.Markup (Markup, text, (!))

pureScriptIntro :: forall e. Markup e
pureScriptIntro = do
  section $ do
    h1 $ text "PureScript type system"
    h2 $ text "part i"
  section $ do
    h2 $ text "PureScript"
    p $ text "A strongly-typed functional programming language that compiles to JavaScript"
    p $ text "Heavily inspired by Haskell."
    ul ! style "font-size: .8em" $ do
      li $ text "Type inference"
      li $ text "Records and row polymorphism"
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
    pre $ code ! className "language-haskell" $ text """a :: Record ( a :: Int, b :: String )
a = { a: 1, b: "" }

b :: { a :: Int, b :: String }
b = { a: 2, b: "a" }
"""
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
main
  :: forall e
   . Eff
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
class RowToList
        (row :: # Type)
        (list :: RowList)
        | row -> list"""
    p $ text "type level prolog"
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
> true :: Gt (S (S Z)) (S (S Z)) => Boolean
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
