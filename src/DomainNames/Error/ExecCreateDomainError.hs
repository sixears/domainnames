module DomainNames.Error.ExecCreateDomainError
  ( ExecCreateDomainError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), (&) )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣), (⊢) )
import Data.MoreUnicode.Maybe  ( pattern 𝕵, pattern 𝕹 )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError ) )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ) )
import ProcLib.Error.ExecCreateError  ( ExecCreateError( ECExecE, ECCreateE ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Error.DomainError  ( AsDomainError( _DomainError )
                                      , DomainError )
--------------------------------------------------------------------------------

data ExecCreateDomainError = ECDExecCreateE ExecCreateError
                           | ECDDomainE     DomainError
  deriving (Eq,Show)

instance Exception ExecCreateDomainError

instance HasCallstack ExecCreateDomainError where
  callstack = lens (\ case ECDExecCreateE ece → ece ⊣ callstack
                           ECDDomainE     de  → de  ⊣ callstack)
                   (\ ecde cs → case ecde of
                                  ECDExecCreateE ece →
                                    ECDExecCreateE $ ece & callstack ⊢ cs
                                  ECDDomainE de →
                                    ECDDomainE $ de & callstack ⊢ cs
                   )

instance Printable ExecCreateDomainError where
  print (ECDExecCreateE e) = P.string (show e)
  print (ECDDomainE e)     = print e

_ECDExecCreateE ∷ Prism' ExecCreateDomainError ExecCreateError
_ECDExecCreateE = prism' ECDExecCreateE
                         (\ case (ECDExecCreateE e) → 𝕵 e; _ → 𝕹)

instance AsExecError ExecCreateDomainError where
  _ExecError = prism' (ECDExecCreateE ∘ ECExecE)
                      (\ case (ECDExecCreateE (ECExecE e)) → 𝕵 e
                              _                              → 𝕹)
instance AsCreateProcError ExecCreateDomainError where
  _CreateProcError = prism' (ECDExecCreateE ∘ ECCreateE)
                            (\ case (ECDExecCreateE (ECCreateE e)) → 𝕵 e
                                    _                                → 𝕹)

instance AsDomainError ExecCreateDomainError where
  _DomainError = prism' ECDDomainE
                        (\ case (ECDDomainE e) → 𝕵 e; _ → 𝕹)

-- that's all, folks! ----------------------------------------------------------
