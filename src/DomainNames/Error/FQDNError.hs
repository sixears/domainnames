module DomainNames.Error.FQDNError
  ( AsFQDNError( _FQDNError ), FQDNError( FQDNNotFullyQualifiedErr )
  , ToFQDNError( toFQDNError ), throwAsFQDNError )
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

data FQDNError = FQDNNotFullyQualifiedErr Text CallStack
               | DomainErrorErr DomainError
  deriving Show

instance Exception FQDNError

instance Eq FQDNError where
  (FQDNNotFullyQualifiedErr t1 _) == (FQDNNotFullyQualifiedErr t2 _) = t1 == t2
  (DomainErrorErr e1) == (DomainErrorErr e2) = e1 == e2
  _ == _ = 𝕱

instance HasCallstack FQDNError where
  callstack = lens (\ case FQDNNotFullyQualifiedErr _ cs → cs
                           DomainErrorErr de             → de ⊣ callstack)
                   (\ fe cs →
                      case fe of
                        FQDNNotFullyQualifiedErr t _ →
                          FQDNNotFullyQualifiedErr t cs
                        DomainErrorErr de → DomainErrorErr $ de & callstack ⊢ cs
                   )

instance Printable FQDNError where
  print (FQDNNotFullyQualifiedErr t _) =
    P.text $ [fmt|FQDN not fully qualified: '%t'|] t
  print (DomainErrorErr e) = print e

_FQDNNotFullyQualifiedErr ∷ Prism' FQDNError Text
_FQDNNotFullyQualifiedErr = prism' (\ t → FQDNNotFullyQualifiedErr t callStack)
                                   ( \ case
                                         (FQDNNotFullyQualifiedErr t _) → 𝕵 t
                                         _                              → 𝕹
                                   )

_DomainErrorErr ∷ Prism' FQDNError DomainError
_DomainErrorErr = prism' DomainErrorErr
                         (\ case (DomainErrorErr e) → 𝕵 e; _ → 𝕹)

--------------------

class AsFQDNError ε where
  _FQDNError ∷ Prism' ε FQDNError

instance AsFQDNError FQDNError where
  _FQDNError = id

instance AsDomainError FQDNError where
  _DomainError = prism' DomainErrorErr (⩼ _DomainErrorErr)

--------------------

class ToFQDNError α where
  toFQDNError ∷ α → FQDNError

instance ToFQDNError FQDNError where
  toFQDNError = id

instance ToFQDNError DomainError where
  toFQDNError = DomainErrorErr

throwAsFQDNError ∷ (ToFQDNError α, AsFQDNError ε, MonadError ε η) ⇒ α → η β
throwAsFQDNError = throwError ∘ (_FQDNError #) ∘ toFQDNError

-- that's all, folks! ----------------------------------------------------------
