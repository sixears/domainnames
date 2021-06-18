module DomainNames.Error.DomainError
  ( AsDomainError( _DomainError ), DomainError( DomainEmptyErr
                                              , DomainLengthErr )
  , throwAsDomainError, toDomainError
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), (&),const, id )
import GHC.Stack          ( CallStack, HasCallStack, callStack )
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

import Data.MoreUnicode.Bool   ( pattern 𝕱, pattern 𝕿 )
import Data.MoreUnicode.Lens   ( (⊣), (⊢) )
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

import DomainNames.Error.DomainLabelError
                              ( AsDomainLabelError( _DomainLabelError )
                              , DomainLabelError )

--------------------------------------------------------------------------------

data DomainError = DomainEmptyErr CallStack
                 | DomainLengthErr Text CallStack
                 | DomainLabelErr  DomainLabelError
  deriving Show

instance Exception DomainError

instance Eq DomainError where
  (DomainEmptyErr _)      == (DomainEmptyErr _)     = 𝕿
  (DomainLengthErr t1 _)  == (DomainLengthErr t2 _) = t1 == t2
  (DomainLabelErr e1)  == (DomainLabelErr e2)       = e1 == e2
  _ == _ = 𝕱

instance HasCallstack DomainError where
  callstack = lens (\ case DomainEmptyErr    cs → cs
                           DomainLengthErr _ cs → cs
                           DomainLabelErr  e    → e ⊣ callstack
                   )
                   (\ de cs → case de of
                                DomainEmptyErr  _   → DomainEmptyErr cs
                                DomainLengthErr t _ → DomainLengthErr t cs
                                DomainLabelErr  e   →
                                  DomainLabelErr $ e & callstack ⊢ cs
                   )

instance Printable DomainError where
  print (DomainEmptyErr _)    = P.text "empty domain"
  print (DomainLengthErr t _) = P.text $ [fmt|domain too long: '%t'|] t
  print (DomainLabelErr e)    = print e

_DomainEmptyErr ∷ Prism' DomainError ()
_DomainEmptyErr = prism' (const $ DomainEmptyErr callStack)
                         ( \ case DomainEmptyErr _ → 𝕵 (); _ → 𝕹 )

_DomainLabelErr ∷ Prism' DomainError DomainLabelError
_DomainLabelErr = prism' (\ t → DomainLabelErr t)
                         (\ case DomainLabelErr t → 𝕵 t; _ → 𝕹)

_DomainLengthErr ∷ Prism' DomainError Text
_DomainLengthErr = prism' (\ t → DomainLengthErr t callStack)
                          (\ case DomainLengthErr t _ → 𝕵 t; _ → 𝕹)

--------------------

class AsDomainError ε where
  _DomainError ∷ Prism' ε DomainError

instance AsDomainError DomainError where
  _DomainError = id

instance AsDomainLabelError DomainError where
  _DomainLabelError = prism' (\ dle → DomainLabelErr dle)
                             (\ case DomainLabelErr e → 𝕵 e; _ → 𝕹)

--------------------

class ToDomainError α where
  toDomainError ∷ HasCallStack ⇒ α → DomainError

instance ToDomainError DomainError where
  toDomainError = id

instance ToDomainError DomainLabelError where
  toDomainError x = DomainLabelErr x

throwAsDomainError ∷ (ToDomainError α, AsDomainError ε, MonadError ε η) ⇒ α → η β
throwAsDomainError = throwError ∘ (_DomainError #) ∘ toDomainError

-- that's all, folks! ----------------------------------------------------------
