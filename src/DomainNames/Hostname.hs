module DomainNames.Hostname
  ( Hostname, Localname
  , (<.>), (<..>)
  , checkWL, checkWL', filterWL, host, hostlocal, hostname, localname
  , parseHostname, parseHostname', __parseHostname, __parseHostname'
  , parseLocalname, parseLocalname'
  )
where

import Prelude  ( error )

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Monad       ( fail, return )
import Data.Either         ( either, fromRight )
import Data.Eq             ( Eq )
import Data.Function       ( ($), (&) )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )
import Data.Maybe          ( Maybe( Just, Nothing ) )
import Data.Ord            ( Ord, max, min )
import Data.String         ( String )
import Data.Tuple          ( swap )
import GHC.Generics        ( Generic )
import GHC.Stack           ( callStack )
import Text.Show           ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Map  as  Map

import Data.Map  ( mapAccumWithKey )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import Dhall  ( FromDhall( autoWith ) )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- ip4 ---------------------------------

import IP4  ( IP4 )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( from, iso )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵 )
import Data.MoreUnicode.Monoid   ( ф )
import Data.MoreUnicode.Lens     ( (⊣), (⊩) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.NonEmptyHashSet  ( NonEmptyHashSet, toNEList )

-- quasiquoting ------------------------

import QuasiQuoting  ( exp, mkQQ )

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ, appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, unsnoc )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), Value( String ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Domain               ( DomainLabel
                                        , IsDomainLabels( dLabels
                                                        , domainLabels )
                                        , domainHead, prepend, parseDomainLabel'
                                        )
import DomainNames.Error.DomainError    ( AsDomainError
                                        , DomainError( DomainEmptyErr ) )
import DomainNames.Error.LocalnameError ( AsLocalnameError, LocalnameError
                                        , throwAsLocalnameError )
import DomainNames.FQDN                 ( FQDN, parseFQDN' )

import DomainNames.Error.HostnameError
  ( AsHostnameError, HostnameError( HostnameNotFullyQualifiedE )
  , throwAsHostnameError )

--------------------------------------------------------------------------------

newtype Localname = Localname DomainLabel
  deriving (Eq, Generic, Hashable, NFData, Show)

instance FromDhall Localname where
  autoWith iopts = __parseLocalname' ⊳ autoWith iopts

instance Printable Localname where
  print (Localname d) = print d

instance FromJSON Localname where
  parseJSON (String t) = either (fail ∘ toString) return $ parseLocalname' t
  parseJSON invalid    = typeMismatch "localname" invalid

parseLocalname ∷ (Printable ρ, AsLocalnameError ε, MonadError ε η) ⇒
                 ρ → η Localname
parseLocalname (toText → t) =
  either throwAsLocalnameError (return ∘ Localname) $ parseDomainLabel' t

parseLocalname' ∷ (Printable ρ, MonadError LocalnameError η) ⇒ ρ → η Localname
parseLocalname' = parseLocalname

__parseLocalname ∷ Printable ρ ⇒ ρ → Localname
__parseLocalname = fromRight (error "not a Right") ∘ parseLocalname'

__parseLocalname' ∷ Text → Localname
__parseLocalname' = __parseLocalname

localname ∷ QuasiQuoter
localname = let parseExp ∷ String → 𝕄 ExpQ
                parseExp = 𝕵 ∘ appE (varE '__parseLocalname') ∘ litE ∘ stringL
             in mkQQ "local" $ def & exp ⊩ parseExp

------------------------------------------------------------

newtype Hostname = Hostname { unHostname ∷ FQDN }
  deriving (Eq, Generic, Hashable, NFData, Ord, Show)

instance Printable Hostname where
  print (Hostname fq) = print fq

instance IsDomainLabels Hostname where
  domainLabels = iso (view domainLabels ∘ unHostname) (Hostname ∘ view (from domainLabels))

----------------------------------------

hostlocal ∷ Hostname → Localname
hostlocal (Hostname h) = Localname (domainHead h)

----------------------------------------

instance FromDhall Hostname where
  autoWith iopts = __parseHostname' ⊳ autoWith iopts

instance FromJSON Hostname where
  parseJSON (String t) = either (fail ∘ toString) return $ parseHostname' t
  parseJSON invalid    = typeMismatch "hostname" invalid

parseHostname ∷ (Printable ρ, AsHostnameError ε, MonadError ε η) ⇒
                ρ → η Hostname
parseHostname (toText → t) =
  case unsnoc t of
    Nothing →
      throwAsHostnameError $ DomainEmptyErr callStack
    Just (_,'.') →
      either throwAsHostnameError (return ∘ Hostname) $ parseFQDN' t
    Just (_,_) →
      throwAsHostnameError $ HostnameNotFullyQualifiedE t callStack

parseHostname' ∷ (Printable ρ, MonadError HostnameError η) ⇒ ρ → η Hostname
parseHostname' = parseHostname

__parseHostname ∷ Printable ρ ⇒ ρ → Hostname
__parseHostname = fromRight (error "not a Right") ∘ parseHostname'

__parseHostname' ∷ Text → Hostname
__parseHostname' = __parseHostname

hostname ∷ QuasiQuoter
hostname = let parseExp ∷ String → 𝕄 ExpQ
               parseExp = 𝕵 ∘ appE (varE '__parseHostname') ∘ litE ∘ stringL
            in mkQQ "hostname" $ def & exp ⊩ parseExp


host ∷ QuasiQuoter
host = hostname
-- h ∷ QuasiQuoter
-- h = hostname

(<.>) ∷ MonadError DomainError η ⇒ Localname → FQDN → η Hostname
(<.>) = (<..>)

(<..>) ∷ (AsDomainError ε, MonadError ε η) ⇒ Localname → FQDN → η Hostname
(Localname d) <..> f = Hostname ⊳ (prepend d f)

----------------------------------------

-- given two hostnames; if one is the other+"-wl", then return the base
-- name - else return the first name, and an error
checkWL' ∷ Hostname → Hostname → ([Text],Hostname)
checkWL' h1 h2 =
  let (l1 :| d1) = h1 ⊣ dLabels
      (l2 :| d2) = h2 ⊣ dLabels
      errNm = [fmt|names are not "x" vs. "x-wl": '%T' vs. '%T'|] (min h1 h2)
                                                                 (max h1 h2)
      errDm = [fmt|different domains: '%T' vs. '%T'|] h1 h2
   in if d1 ≡ d2
      then if toText l1 ≡ toText l2 ⊕ "-wl"
           then (ф,h2)
           else if toText l2 ≡ toText l1 ⊕ "-wl"
                then (ф,h1)
                else ([errNm], min h1 h2)
      else ([errDm], min h1 h2)

--------------------

{- | Check that ip4, {hostnames} is actually pair of hostnames where one
     is the other + "-wl"; return the base name; or else add an error.
     The IP is passed just for the errmsg
 -}
checkWL ∷ IP4 → NonEmptyHashSet Hostname → ([Text], Hostname)
checkWL i hh = let errTooMany l = [fmt|too many hosts for IP %T (%L)|] i l
                in case toNEList hh of
                     h  :| []     → (ф,h)
                     h1 :| [h2]   → checkWL' h1 h2
                     lh@(h1 :| _) → ([errTooMany lh], h1)

----------------------------------------

{- | Check that the map ip4 -> hostnames has only pairs of hostnames where one
     is the other + "-wl"; return the base name in each case (and errors for
     ip->{many hostnames} that don't fit that rule).
 -}
filterWL ∷ Map.Map IP4 (NonEmptyHashSet Hostname)
         → (Map.Map IP4 Hostname, [Text])
filterWL = let accumulator es i hh = let (es',h) = checkWL i hh in (es'⊕es, h)
            in swap ∘ mapAccumWithKey accumulator ф



-- that's all, folks! ----------------------------------------------------------
