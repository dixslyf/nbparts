module Tests.SpecHook where

import Test.Hspec (Spec, parallel)

hook :: Spec -> Spec
hook = parallel
