{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- base --------------------------------

import Control.Monad  ( return )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.Tasty        ( runTests_, tastyOptParser )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( failureCode, fullDesc, info, prefs
                                    , progDesc, showHelpOnError )
import Options.Applicative.Extra    ( customExecParser, helper )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  DomainNames.T.FQDN
import qualified  DomainNames.T.Hostname

--------------------------------------------------------------------------------

tests ∷ TestTree

tests = testGroup "domainnames" [ DomainNames.T.FQDN.tests
                                , DomainNames.T.Hostname.tests
                                ]

main ∷ IO ()
main = do
  tastyOpts ← customExecParser (prefs showHelpOnError) $
                info (helper ⊵ tastyOptParser tests)
                     (fullDesc ⊕ progDesc "tests for domainnames package"
                               ⊕ failureCode 254)


  _ ← runTests_ tastyOpts
  return ()
