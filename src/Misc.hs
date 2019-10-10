module Misc
    ( topAdderTree
    )
where

import           Clash.Prelude
import           Data.Proxy
import           Data.Kind
import           Data.Singletons.Prelude        ( TyFun
                                                , Apply
                                                , type (@@)
                                                )

data Sum (n :: Nat) (f :: TyFun Nat Type) :: Type
type instance Apply (Sum n) l = Signed (n + l)

adderTree :: KnownNat k => Vec (2 ^ k) (Signed n) -> Signed (n + k)
adderTree = dtfold (Proxy :: Proxy (Sum n)) id $ \_ x y -> add x y

{-# ANN topAdderTree (defSyn "topAdderTree") #-}
topAdderTree :: Clock System -> Reset System -> Enable System -> Signal System (Vec 32 (Signed 16)) -> Signal System (Signed 21)
topAdderTree = exposeClockResetEnable (register 0 . fmap adderTree)
{-# NOINLINE topAdderTree #-}
