module DomainNames.Error.UQDNError
  ( AsUQDNError, UQDNError( UQDNFullyQualifiedErr ), throwAsUQDNError )
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

import DomainNames.Error.DomainError  ( AsDomainError( _DomainError )
                                      , DomainError )

--------------------------------------------------------------------------------

data UQDNError = UQDNFullyQualifiedErr Text CallStack
               | DomainErrorErr DomainError
  deriving Show

--------------------

instance Exception UQDNError

--------------------

instance Eq UQDNError where
  (UQDNFullyQualifiedErr t1 _) == (UQDNFullyQualifiedErr t2 _) = t1  == t2
  (DomainErrorErr        de1)  == (DomainErrorErr        de2)  = de1 == de2
  _                            == _                            = 𝕱

--------------------

instance HasCallstack UQDNError where
  callstack = lens (\ case UQDNFullyQualifiedErr _ cs → cs
                           DomainErrorErr de          → de ⊣ callstack)
                   (\ ue cs → case ue of UQDNFullyQualifiedErr t _ →
                                           UQDNFullyQualifiedErr t cs
                                         DomainErrorErr de →
                                           DomainErrorErr $ de & callstack ⊢ cs
                   )

--------------------

instance Printable UQDNError where
  print (UQDNFullyQualifiedErr t _) =
    P.text $ [fmt|UQDN fully qualified: '%t'|] t
  print (DomainErrorErr e) = print e

--------------------

_UQDNNotFullyQualifiedErr ∷ Prism' UQDNError Text
_UQDNNotFullyQualifiedErr = prism' (\ t → UQDNFullyQualifiedErr t callStack)
                                   ( \ case
                                         (UQDNFullyQualifiedErr t _) → 𝕵 t
                                         _                           → 𝕹
                                   )

--------------------

_DomainErrorErr ∷ Prism' UQDNError DomainError
_DomainErrorErr = prism' DomainErrorErr
                         (\ case (DomainErrorErr e) → 𝕵 e; _ → 𝕹)

------------------------------------------------------------

class AsUQDNError ε where
  _UQDNError ∷ Prism' ε UQDNError

instance AsUQDNError UQDNError where
  _UQDNError = id

instance AsDomainError UQDNError where
  _DomainError = prism' DomainErrorErr (⩼ _DomainErrorErr)

--------------------

class ToUQDNError α where
  toUQDNError ∷ α → UQDNError

instance ToUQDNError UQDNError where
  toUQDNError = id

instance ToUQDNError DomainError where
  toUQDNError = DomainErrorErr

throwAsUQDNError ∷ (ToUQDNError α, AsUQDNError ε, MonadError ε η) ⇒ α → η β
throwAsUQDNError = throwError ∘ (_UQDNError #) ∘ toUQDNError

-- that's all, folks! ----------------------------------------------------------
