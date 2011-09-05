{-|
    Module      :  PolyPaver
    Description :  deciding real inequalities and interval inclusions 
    Copyright   :  (c) Jan Duracz, Michal Konecny 
    License     :  BSD3

    Maintainer  :  jan@duracz.net
    Stability   :  experimental
    Portability :  portable

    Tools for deciding systems of real inequalities and interval inclusions over bounded domains. 
-}
module PolyPaver
(
    defaultMain,
    Problem(..),
    module PolyPaver.Form
)
where

import PolyPaver.Form
import PolyPaver.Paver
