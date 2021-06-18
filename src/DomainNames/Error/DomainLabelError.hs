{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DomainNames.Error.DomainLabelError
  ( AsDomainLabelError( _DomainLabelError )
  , DomainLabelError( DomainLabelEmptyErr, DomainLabelHyphenFirstCharErr
                    , DomainLabelIllegalCharErr, DomainLabelLengthErr )
  , throwAsDomainLabelError
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Bool          ( Bool( False, True ) )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), id )
import GHC.Stack          ( CallStack )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text, length )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Constants  ( maxLabelLength )

--------------------------------------------------------------------------------

data DomainLabelError = DomainLabelEmptyErr                CallStack
                      | DomainLabelHyphenFirstCharErr Text CallStack
                      | DomainLabelLengthErr          Text CallStack
                      | DomainLabelIllegalCharErr     Text CallStack
  deriving Show

instance Exception DomainLabelError

instance Eq DomainLabelError where
  (DomainLabelEmptyErr _)      == (DomainLabelEmptyErr _)     = True
  (DomainLabelHyphenFirstCharErr t1 _) == (DomainLabelHyphenFirstCharErr t2 _) =
    t1 == t2
  (DomainLabelLengthErr t1 _)  == (DomainLabelLengthErr t2 _) = t1 == t2
  (DomainLabelIllegalCharErr t1 _)  == (DomainLabelIllegalCharErr t2 _) =
    t1 == t2
  _ == _ = False

instance HasCallstack DomainLabelError where
  callstack = lens (\ case DomainLabelEmptyErr             cs → cs
                           DomainLabelHyphenFirstCharErr _ cs → cs
                           DomainLabelLengthErr          _ cs → cs
                           DomainLabelIllegalCharErr     _ cs → cs
                   )
                   (\ dle cs →
                        case dle of
                          DomainLabelEmptyErr _ →
                            DomainLabelEmptyErr cs
                          DomainLabelHyphenFirstCharErr t _ →
                            DomainLabelHyphenFirstCharErr t cs
                          DomainLabelLengthErr t _ →
                            DomainLabelLengthErr t cs
                          DomainLabelIllegalCharErr t _ →
                            DomainLabelIllegalCharErr t cs
                   )

instance Printable DomainLabelError where
  print (DomainLabelEmptyErr _) = P.text "empty domain label"
  print (DomainLabelHyphenFirstCharErr d _) = P.text $
    [fmt|domain label first character must not be a hyphen '%t'|] d
  print (DomainLabelLengthErr d _) = P.text $
    [fmt|domain label length %d exceeds %d '%t'|] (length d) maxLabelLength d
  print (DomainLabelIllegalCharErr d _) = P.text $
    [fmt|domain label characters must be alpha-numeric or hyphen '%t'|] d

--------------------

class AsDomainLabelError ε where
  _DomainLabelError ∷ Prism' ε DomainLabelError

instance AsDomainLabelError DomainLabelError where
  _DomainLabelError = id

throwAsDomainLabelError ∷ (AsDomainLabelError ε, MonadError ε η) ⇒
                          DomainLabelError → η α
throwAsDomainLabelError = throwError ∘ (_DomainLabelError #)

-- that's all, folks! ----------------------------------------------------------
