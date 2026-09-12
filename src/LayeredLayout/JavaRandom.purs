-- | Faithful PureScript port of `java.util.Random`.
-- ELK uses Java's Random for tiebreaking in crossing minimization;
-- to match ELK's output we replicate the exact bit-for-bit sequence.
--
-- 48-bit linear congruential generator with state s' = (s * 0x5DEECE66D + 0xB) mod 2^48.
-- We use BigInt for exact 48-bit arithmetic.
module LayeredLayout.JavaRandom
  ( Random
  , mkRandom
  , mkRandomBI
  , next
  , nextInt
  , nextDouble
  , nextLong
  , nextLongBI
  , nextBoolean
  , randomShuffle
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Int.Bits (and)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Number (pow)
import JS.BigInt (BigInt)
import JS.BigInt as BI

-- | A Java Random instance: 48-bit seed.
newtype Random = Random BigInt

-- | Create a Random initialized like `new java.util.Random(seed)`.
-- Java does: this.seed = (seed XOR 0x5DEECE66D) AND ((1 << 48) - 1)
mkRandom :: Number -> Random
mkRandom seed = mkRandomBI (numToBigInt seed)

-- | BigInt-precision version of `mkRandom`. Use this when threading a
-- 64-bit `nextLong()` result back into a `setSeed` to avoid Number's
-- 53-bit mantissa truncating bits 53-63 of the long.
mkRandomBI :: BigInt -> Random
mkRandomBI seed = Random (BI.and (BI.xor seed multiplier) mask48)

-- | Java's Random.next(bits): advance state, return top `bits` of state.
next :: Int -> Random -> Int /\ Random
next bits (Random s) = do
  let s' = BI.and (s * multiplier + increment) mask48
  let shifted = BI.shr s' (BI.fromInt (48 - bits))
  let result = fromMaybe 0 (Int.fromNumber (BI.toNumber shifted))
  result /\ Random s'

-- java.util.Random.nextInt(bound), including rejection rather than modulo bias.
nextInt :: Int -> Random -> Int /\ Random
nextInt bound random =
  let
    bits /\ random' = next 31 random
    result = bits `mod` bound
  in
    if and bound (bound - 1) == 0 then Int.floor (Int.toNumber bound * Int.toNumber bits / 2147483648.0) /\ random'
    else if Int.toNumber bits - Int.toNumber result + Int.toNumber (bound - 1) >= 2147483648.0 then nextInt bound random'
    else result /\ random'

-- | nextDouble() = ((next(26) << 27) | next(27)) / 2^53
nextDouble :: Random -> Number /\ Random
nextDouble r = do
  let high /\ r1 = next 26 r
  let low /\ r2 = next 27 r1
  let combined = Int.toNumber high * pow 2.0 27.0 + Int.toNumber low
  (combined / pow 2.0 53.0) /\ r2

-- | nextLong() = (next(32) << 32) + next(32)
-- |
-- | NOTE: Java's `nextLong()` returns a 64-bit signed long. Number can
-- | only represent integers exactly up to 2^53; for larger magnitudes
-- | (like Random(1).nextLong() = -4964420948893066024) low-order bits
-- | are silently dropped. Use `nextLongBI` if the result is going to
-- | be re-fed into `setSeed`/`mkRandom`.
nextLong :: Random -> Number /\ Random
nextLong r = do
  let high /\ r1 = next 32 r
  let low /\ r2 = next 32 r1
  (Int.toNumber high * pow 2.0 32.0 + Int.toNumber low) /\ r2

-- | BigInt-precision version of `nextLong`. Returns the exact 64-bit
-- | signed integer value as a BigInt, suitable for feeding back into
-- | `mkRandomBI` without Number precision loss.
-- |
-- | Java: `((long)next(32) << 32) + next(32)` — both halves are signed
-- | 32-bit, so the high half has its top bit interpreted as the sign of
-- | the resulting 64-bit value.
nextLongBI :: Random -> BigInt /\ Random
nextLongBI (Random s) = do
  let s1 = BI.and (s * multiplier + increment) mask48
  let high32 = BI.shr s1 (BI.fromInt 16) -- top 32 bits, in [0, 2^32)
  let s2 = BI.and (s1 * multiplier + increment) mask48
  let low32 = BI.shr s2 (BI.fromInt 16) -- top 32 bits of s2
  -- Sign-extend the low half so the combined value matches Java's signed addition.
  let signedHigh = toSignedI32 high32
  let signedLow = toSignedI32 low32
  let combined = (signedHigh * twoPow32) + signedLow
  combined /\ Random s2

-- | Reinterpret a non-negative BigInt in [0, 2^32) as a signed 32-bit integer
-- | (treating bit 31 as the sign bit), matching Java's `(int)` cast semantics.
toSignedI32 :: BigInt -> BigInt
toSignedI32 b = if b >= twoPow31 then b - twoPow32 else b

twoPow31 :: BigInt
twoPow31 = BI.shl one (BI.fromInt 31)

twoPow32 :: BigInt
twoPow32 = BI.shl one (BI.fromInt 32)

-- | nextBoolean() = next(1) /= 0
nextBoolean :: Random -> Boolean /\ Random
nextBoolean r = do
  let n /\ r' = next 1 r
  (n /= 0) /\ r'

-- | Shuffle by sorting on random barycenters (ELK's randomizeBarycenters).
randomShuffle :: forall a. Random -> Array a -> Array a /\ Random
randomShuffle r0 xs = do
  let { rs, finalR } = foldl step { rs: [], finalR: r0 } xs
  let keyed = A.zipWith (\x k -> { x, k }) xs rs
  let sorted = A.sortBy (\a b -> compare a.k b.k) keyed
  (sorted <#> _.x) /\ finalR
  where
  step { rs, finalR } _ = do
    let v /\ r' = nextDouble finalR
    { rs: rs <> [ v ], finalR: r' }

-- ── Internal constants ──────────────────────────────────────────

multiplier :: BigInt
multiplier = fromMaybe zero (BI.fromString "25214903917") -- 0x5DEECE66D

increment :: BigInt
increment = BI.fromInt 11

mask48 :: BigInt
mask48 = BI.shl one (BI.fromInt 48) - one

-- | Convert a Number to BigInt (truncating any fractional part).
numToBigInt :: Number -> BigInt
numToBigInt n = fromMaybe zero (BI.fromNumber n)
