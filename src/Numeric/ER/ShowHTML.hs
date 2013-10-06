{-|
    Module      :  Numeric.ER.ShowHTML
    Description :  Misc facilities for HTML rendering.
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
     
-}
module Numeric.ER.ShowHTML where

import qualified Text.Html as H
import Text.Regex

{-|
    Render HTML is a way that can be inlined in 
    Javascript strings etc.
-}
showHTML :: 
    (H.HTML t) =>
    t -> String
showHTML v =
    escapeNewLines $
    renderHtmlNoHeader $ 
    H.toHtml v
    where
--    stripHeader s =
--        (splitRegex (mkRegex "-->") s) !! 1
    escapeNewLines s =
        (subRegex (mkRegex "([^\\])$") s "\\1\\\\")  

abovesTable attrs cells =
    H.table H.! attrs H.<< (H.aboves $ map (H.td H.<<) cells)
besidesTable attrs cells =
    H.table H.! attrs H.<< (H.aboves [H.besides $ map (H.td H.<<) cells])

renderHtmlNoHeader :: H.Html -> String
renderHtmlNoHeader theHtml =
         foldr (.) id (map (H.renderHtml' 0)
                           (H.getHtmlElements theHtml)) "\n"

toHtmlDefault :: (Show a) => a -> H.Html
toHtmlDefault = H.toHtml . show

instance (H.HTML a) => H.HTML (Maybe a) where
    toHtml Nothing = H.toHtml $ "[Nothing]"
    toHtml (Just a) = H.toHtml a

