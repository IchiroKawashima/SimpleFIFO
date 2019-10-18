{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}

module Misc
    ( msu
    , sft
    , fadd
    , fmul
    )
where

import           Clash.Prelude
import           Data.Proxy
import           Data.Kind
import           Data.Singletons.Prelude        ( TyFun
                                                , Apply
                                                , type (@@)
                                                )

{-# ANN msu (defSyn "msu") #-}
msu :: Unsigned 1024 -> Unsigned 1024
msu = last . generate t (\x -> resize $ mul x x `mod` resize n)
  where
    n :: Unsigned 1024
    n = 124066695684124741398798927404814432744698427125735684128131855064976895337309138910015071214657674309443149407457493434579063840841220334555160125016331040933690674569571217337630239191517205721310197608387239846364360850220896772964978569683229449266819903414117058030106528073928633017118689826625594484331
    t :: SNat 10
    t = SNat
{-# NOINLINE msu #-}

{-# ANN sft (defSyn "sft") #-}
sft :: BitVector 32 -> BitVector 5
sft = shifter
{-# NOINLINE sft #-}

data Shifter (k :: Nat) (f :: TyFun Nat Type) :: Type
type instance Apply (Shifter k) l = (BitVector l, BitVector (2 ^ (k - l)))

shifter :: (KnownNat k) => BitVector (2 ^ k) -> BitVector k
shifter x = fst $ dfold' (Proxy :: Proxy (Shifter k)) f (def, x) $ repeat ()
  where
    f SNat () (shift, remnant)
        | bitToBool $ reduceOr lowerBits = (shift ++# 0, lowerBits)
        | otherwise                      = (shift ++# 1, higherBits)
        where (higherBits, lowerBits) = split remnant

{-# ANN fadd (defSyn "fadd") #-}
fadd :: Vec 8 (Signed 32) -> Vec 1 (Signed 35)
fadd = foldAdd
{-# NOINLINE fadd #-}

data FoldAdd (k :: Nat) (n :: Nat) (f :: TyFun Nat Type) :: Type
type instance Apply (FoldAdd k n) l = Vec (2 ^ (k - l)) (Signed (n + l))

foldAdd :: (KnownNat k) => Vec (2 ^ k) (Signed n) -> Vec 1 (Signed (n + k))
foldAdd xs = dfold' (Proxy :: Proxy (FoldAdd k n)) f xs $ repeat ()
    where f SNat () = uncurry (zipWith add) . splitAtI

{-# ANN fmul (defSyn "fmul") #-}
fmul :: Vec 8 (Signed 32) -> Vec 1 (Signed 256)
fmul = foldMul
{-# NOINLINE fmul #-}

data FoldMul (k :: Nat) (n :: Nat) (f :: TyFun Nat Type) :: Type
type instance Apply (FoldMul k n) l = Vec (2 ^ (k - l)) (Signed (n * 2 ^ l))

foldMul :: (KnownNat k) => Vec (2 ^ k) (Signed n) -> Vec 1 (Signed (n * 2 ^ k))
foldMul xs = dfold' (Proxy :: Proxy (FoldMul k n)) f xs $ repeat ()
    where f SNat () = uncurry (zipWith mul) . splitAtI

dfold'
    :: forall p k a
     . (KnownNat k)
    => Proxy (p :: TyFun Nat Type -> Type)
    -> (  forall l
        . (KnownNat l, l <= k - 1, l + 1 <= k)
       => SNat l
       -> a
       -> p @@ l
       -> p @@ (l + 1)
       )
    -> (p @@ 0)
    -> Vec k a
    -> (p @@ k)
dfold' Proxy f z = go
  where
    go :: (KnownNat n, n <= k) => Vec n a -> p @@ n
    go Nil           = z
    go (y `Cons` ys) = f (lengthS ys) y (go ys)
{-# NOINLINE dfold' #-}
