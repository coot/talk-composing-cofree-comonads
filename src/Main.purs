module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (foldFree)
import Control.Monad.State (State, execState, state)
import Data.CatList (CatList)
import Data.Foldable (fold)
import Data.Monoid (append)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Node.Buffer (BUFFER, fromString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (writeFile)
import Prelude (type (~>), Unit, bind, discard, map, pure, ($), (<<<), (<>), (==), (>), (||))
import Text.Smolder.HTML hiding (style, map)
import Text.Smolder.HTML.Attributes (className, href, lang, rel, src, target)
import Text.Smolder.Markup (Attr(..), Markup, MarkupM(..), text, (!))
import ComposingCofree.PureScriptIntro (pureScriptIntro)
import ComposingCofree.ComposingCofreePart (composingCofreePart)

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
        pureScriptIntro
        composingCofreePart
        section $ do
          h1 $ text "Thank You"
          p ! className "footer" $ do
            text "written with "
            a ! href "https://github.com/bodil/purescript-smolder" $ text "purescript-smolder"
            text " and "
            a ! href "https://github.com/hakimel/reveal.js" $ text "reveal.js"
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
