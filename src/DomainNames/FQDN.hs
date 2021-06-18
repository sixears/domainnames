module DomainNames.FQDN
  ( FQDN, fqdn, parseFQDN, parseFQDN' )
where

import Prelude  ( error )

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Either    ( either, fromRight )
import Data.Eq        ( Eq )
import Data.Function  ( ($), (&) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.Ord       ( Ord )
import Data.String    ( String )
import GHC.Stack      ( callStack )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import Dhall  ( FromDhall( autoWith ), Generic )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Iso  ( iso )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵 )
import Data.MoreUnicode.Lens     ( (⊣), (⊩) )

-- quasiquoting ------------------------

import QuasiQuoting    ( exp, mkQQ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ, appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, unsnoc )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), Value( String ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Domain             ( Domain( domainHead, prepend )
                                      , IsDomainLabels( domainLabels )
                                      , DomainLabels, parseDomainLabels'
                                      )
import DomainNames.Error.DomainError  ( DomainError( DomainEmptyErr ) )
import DomainNames.Error.FQDNError    ( AsFQDNError
                                      , FQDNError( FQDNNotFullyQualifiedErr )
                                      , throwAsFQDNError
                                      )

--------------------------------------------------------------------------------

newtype FQDN = FQDN { unFQDN ∷ DomainLabels }
  deriving (Eq, Generic, Hashable, NFData, Ord, Show)

instance IsDomainLabels FQDN where
  domainLabels = iso unFQDN FQDN

instance Domain FQDN where
  prepend d f = FQDN ⊳ (prepend d (f ⊣ domainLabels))
  domainHead (FQDN dls) = domainHead dls

instance Printable FQDN where
  print (FQDN dls) = P.text $ toText dls ⊕ "."

instance FromDhall FQDN where
  autoWith iopts = __parseFQDN' ⊳ autoWith iopts

parseFQDN ∷ (Printable ρ, AsFQDNError ε, MonadError ε η) ⇒ ρ → η FQDN
parseFQDN (toText → t) =
  case unsnoc t of
    Nothing →
      throwAsFQDNError $ DomainEmptyErr callStack
    Just (d,'.') →
      either throwAsFQDNError (return ∘ FQDN) (parseDomainLabels' d)
    Just _ →
      throwAsFQDNError $ FQDNNotFullyQualifiedErr t callStack

parseFQDN' ∷ MonadError FQDNError η ⇒ Text → η FQDN
parseFQDN' = parseFQDN

__parseFQDN ∷ Text → FQDN
__parseFQDN = fromRight (error "not a right") ∘ parseFQDN'

__parseFQDN' ∷ Text → FQDN
__parseFQDN' = __parseFQDN

instance FromJSON FQDN where
  parseJSON (String t) = either (fail ∘ toString) return $ parseFQDN' t
  parseJSON invalid    = typeMismatch "FQDN" invalid

fqdn ∷ QuasiQuoter
fqdn = let parseExp ∷ String → 𝕄 ExpQ
           parseExp = 𝕵 ∘ appE (varE '__parseFQDN') ∘ litE ∘ stringL
        in mkQQ "fqdn" $ def & exp ⊩ parseExp

-- that's all, folks! ----------------------------------------------------------
