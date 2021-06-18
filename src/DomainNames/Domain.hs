module DomainNames.Domain
  ( Domain( (<:>), domainHead, prepend )
  , DomainLabel, DomainLabels, IsDomainLabels( domainLabels, dLabels )
  , dLabel, domainLabel, parseDomainLabel, parseDomainLabel'
  , parseDomainLabels'
  )
where

import Prelude  ( error, fromIntegral )

-- base --------------------------------

import Control.Monad       ( mapM, return )
import Data.Bool           ( not )
import Data.Char           ( isAlphaNum )
import Data.Either         ( Either( Left, Right ), fromRight )
import Data.Eq             ( Eq )
import Data.Foldable       ( toList )
import Data.Function       ( ($), (&), id )
import Data.Functor        ( fmap )
import Data.List.NonEmpty  ( NonEmpty( (:|) ), fromList, head )
import Data.Maybe          ( Maybe( Just, Nothing ) )
import Data.Ord            ( Ord, (>) )
import Data.String         ( String )
import GHC.Stack           ( HasCallStack, callStack )
import Text.Show           ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∨) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import qualified  Dhall  as  D

import Dhall  ( FromDhall( autoWith ), Generic )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( Iso', from, iso )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊩) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- quasiquoting ------------------------

import QuasiQuoting  ( exp, mkQQ )

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ, appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import qualified  Data.Text
import Data.Text  ( Text, any, intercalate, length, uncons )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Constants          ( maxDomainLength, maxLabelLength )
import DomainNames.Error.DomainError  ( AsDomainError
                                      , DomainError( DomainEmptyErr
                                                   , DomainLengthErr )
                                      , throwAsDomainError
                                      )

import DomainNames.Error.DomainLabelError
  ( AsDomainLabelError, DomainLabelError( DomainLabelEmptyErr
                                        , DomainLabelHyphenFirstCharErr
                                        , DomainLabelIllegalCharErr
                                        , DomainLabelLengthErr
                                        )
  , throwAsDomainLabelError
  )

--------------------------------------------------------------------------------

{- | Glossary of Terms:

     * DomainLabel - An identifier, which may contribute to a domain name /
                     hostname.  In the name `www.google.com`, each of `www`,
                     `google`, and `com` are `DomainNamePart`s.
     * Domain      - A non-empty list of domain labels.  A Domain maybe
                     fully-qualified (an FQDN) or unqualified.  FQDNs have the
                     root domain (called '') as the last element, and so are
                     written (and parsed) with a trailing '.'.
     * Hostname    - Any FQDN that may be linked to one or more IPv4 addresses
                     (even if those addresses keep changing, e.g., via
                     round-robin DNS).  E.g., `www.google.com`.
     * LocalName   - The left-most DomainLabel of a Host FQDN (the `head` of the
                     list).  Thus, for the FQDN `www.google.com`; the HostName
                     is `www`.
 -}

------------------------------------------------------------

{-| `Data.Text.splitOn` always returns a non-empty list - even an empty string
    returns a list (`[""]`); we encode that here
-}
splitOn ∷ Text → Text → NonEmpty Text
splitOn s = fromList ∘ Data.Text.splitOn s

------------------------------------------------------------

newtype DomainLabel = DomainLabel Text
  deriving (Eq, Generic, Hashable, NFData, Ord, Show)

instance Printable DomainLabel where
  print (DomainLabel dl) = P.text dl

instance FromDhall DomainLabel where
  autoWith _ = __parseDomainLabel ⊳ D.strictText

------------------------------------------------------------

parseDomainLabel ∷ (Printable ρ, AsDomainLabelError ε, MonadError ε η) ⇒
                   ρ → η DomainLabel
parseDomainLabel (toText → d) =
  case uncons d of
    Nothing       → throwAsDomainLabelError $ DomainLabelEmptyErr callStack
    Just ('-', _) → throwAsDomainLabelError $ DomainLabelHyphenFirstCharErr d callStack
    _             → if any ( \ c → not $ isAlphaNum c ∨ c ≡ '-' ) d
                    then throwAsDomainLabelError $ DomainLabelIllegalCharErr d callStack
                    else if fromIntegral (length d) > maxLabelLength
                         then throwAsDomainLabelError $ DomainLabelLengthErr d callStack
                         else return $ DomainLabel d

parseDomainLabel' ∷ (Printable ρ, MonadError DomainLabelError η) ⇒
                    ρ → η DomainLabel
parseDomainLabel' = parseDomainLabel

__parseDomainLabel ∷ Printable ρ ⇒ ρ → DomainLabel
__parseDomainLabel = fromRight (error "not a right") ∘ parseDomainLabel'

__parseDomainLabel' ∷ Text → DomainLabel
__parseDomainLabel' = __parseDomainLabel

domainLabel ∷ QuasiQuoter
domainLabel =
  let parseExp ∷ String → 𝕄 ExpQ
      parseExp = 𝕵 ∘ appE (varE '__parseDomainLabel') ∘ litE ∘ stringL
  in mkQQ "domainLabel" $ def & exp ⊩ parseExp

dLabel ∷ QuasiQuoter
dLabel = domainLabel
-- dl ∷ QuasiQuoter
-- dl = domainLabel

------------------------------------------------------------

class IsDomainLabels δ where
  domainLabels ∷ Iso' δ DomainLabels
  dLabels      ∷ Iso' δ (NonEmpty DomainLabel)
  dLabels      = iso (view dLabels ∘ view domainLabels)
                     (view (from domainLabels) ∘ view (from dLabels))

newtype DomainLabels = DomainLabels { unDomainLabels ∷ NonEmpty DomainLabel }
  deriving (Eq, Generic, Hashable, NFData, Ord, Show)

instance IsDomainLabels DomainLabels where
  domainLabels = id
  dLabels = iso unDomainLabels DomainLabels

{- | Render a domain, without a trailing dot even for FQDN, to make the 253-char
     check that excludes any trailing dot
-}
renderDomainLabels ∷ NonEmpty DomainLabel → Text
renderDomainLabels ds = Data.Text.intercalate "." (toText ⊳ toList ds)

instance Printable DomainLabels where
  print (DomainLabels dls) = P.text $ renderDomainLabels dls

instance FromDhall DomainLabels where
  autoWith iopts = DomainLabels ⊳ fmap fromList (D.list (autoWith iopts))

----------------------------------------

checkDomainLength ∷ (AsDomainError ε, MonadError ε η) ⇒
                    NonEmpty DomainLabel → η DomainLabels
checkDomainLength dls =
  let txt = renderDomainLabels dls
   in if fromIntegral (length txt) > maxDomainLength
      then throwAsDomainError $ DomainLengthErr txt callStack
      else return $ DomainLabels dls

----------------------------------------

parseDomainLabels ∷ forall ε ρ η .
                    (Printable ρ, AsDomainError ε, MonadError ε η,
                     HasCallStack) ⇒
                    ρ → η DomainLabels
parseDomainLabels (splitOn "." ∘ toText → ("" :| [])) =
  throwAsDomainError $ DomainEmptyErr callStack
parseDomainLabels (splitOn "." ∘ toText → ds) =
  case mapM parseDomainLabel' ds of
    Left  dle → throwAsDomainError dle
    Right ds' → checkDomainLength ds'

parseDomainLabels' ∷ (Printable ρ, MonadError DomainError η) ⇒
                     ρ → η DomainLabels
parseDomainLabels' = parseDomainLabels

------------------------------------------------------------

class IsDomainLabels δ ⇒ Domain δ where
--  domainLabels ∷ δ → DomainLabels
  prepend ∷ (AsDomainError ε, MonadError ε η) ⇒ DomainLabel → δ → η δ
  domainHead ∷ δ → DomainLabel

  (<:>) ∷ MonadError DomainError η ⇒ DomainLabel → δ → η δ
  (<:>) = prepend

instance Domain DomainLabels where
  prepend d  (DomainLabels ds) = checkDomainLength (d :| (toList ds))
  domainHead (DomainLabels ds) = head ds

-- that's all, folks! ----------------------------------------------------------
