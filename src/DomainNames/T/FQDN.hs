module DomainNames.T.FQDN
  ( tests )
where

-- base --------------------------------

import Data.Either         ( Either( Right ) )
import Data.Function       ( ($) )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- more-unicode -----------------------

import Data.MoreUnicode.Functor  ( (⊳) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Domain  ( dLabel, dLabels )
import DomainNames.FQDN    ( parseFQDN' )

--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "FQDN"
    [ testCase "parseFQDN'" $
            Right ([dLabel|foo|] :| [[dLabel|bar|]])
        @=? view dLabels ⊳ parseFQDN' ("foo.bar." ∷ Text)
    ]

-- that's all, folks! ----------------------------------------------------------
