module DomainNames.Error.HostnameError
  ( AsHostnameError, HostnameError( HostnameNotFullyQualifiedE )
  , throwAsHostnameError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), (&), id )
import GHC.Stack          ( CallStack, callStack )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool   ( pattern 𝕱 )
import Data.MoreUnicode.Lens   ( (⊣), (⊢), (⩼) )
import Data.MoreUnicode.Maybe  ( pattern 𝕵, pattern 𝕹 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Error.DomainError  ( DomainError )
import DomainNames.Error.FQDNError    ( AsFQDNError( _FQDNError ), FQDNError
                                      , toFQDNError )

--------------------------------------------------------------------------------

data HostnameError = HostnameNotFullyQualifiedE Text CallStack
                   | HostnameFQDNE FQDNError
  deriving Show

instance Exception HostnameError

instance Eq HostnameError where
  (HostnameNotFullyQualifiedE t1 _) == (HostnameNotFullyQualifiedE t2 _) =
    t1 == t2
  (HostnameFQDNE e1) == (HostnameFQDNE e2) = e1 == e2
  _ == _ = 𝕱

_HostnameNotFullyQualifiedE ∷ Prism' HostnameError Text
_HostnameNotFullyQualifiedE =
  prism' (\ t → HostnameNotFullyQualifiedE t callStack)
         (\ case (HostnameNotFullyQualifiedE h _) → 𝕵 h; _ → 𝕹)

_HostnameFQDNE ∷ Prism' HostnameError FQDNError
_HostnameFQDNE = prism' HostnameFQDNE
                          (\ case (HostnameFQDNE e) → 𝕵 e; _ → 𝕹)

instance Printable HostnameError where
  print (HostnameNotFullyQualifiedE h _) =
    P.text $ [fmt|hostname is not fully qualified: '%t'|] h
  print (HostnameFQDNE e) = print e

instance HasCallstack HostnameError where
  callstack = lens (\ case HostnameNotFullyQualifiedE _ cs → cs
                           HostnameFQDNE              hfe  → hfe ⊣ callstack)
                   (\ he cs → case he of
                                HostnameNotFullyQualifiedE t _ →
                                  HostnameNotFullyQualifiedE t cs
                                HostnameFQDNE hfe →
                                  HostnameFQDNE $ hfe & callstack ⊢ cs
                   )


class AsHostnameError ε where
  _HostnameError ∷ Prism' ε HostnameError

--------------------

class ToHostnameError α where
  toHostnameError ∷ α → HostnameError

instance ToHostnameError HostnameError where
  toHostnameError = id

instance ToHostnameError FQDNError where
  toHostnameError = HostnameFQDNE

instance ToHostnameError DomainError where
  toHostnameError = HostnameFQDNE ∘ toFQDNError

throwAsHostnameError ∷ (ToHostnameError α, AsHostnameError ε, MonadError ε η) ⇒
                       α → η β
throwAsHostnameError = throwError ∘ (_HostnameError #) ∘ toHostnameError

instance AsHostnameError HostnameError where
  _HostnameError = id

instance AsFQDNError HostnameError where
  _FQDNError = prism' HostnameFQDNE (⩼ _HostnameFQDNE)

-- that's all, folks! ----------------------------------------------------------
