module Data.ByteString
  ( ByteString
  , ReadResult(..)
  , empty
  , length
  , unsafeFreeze
  , flush
  , splitAt
  , take
  , take'
  , drop
  , fromInt8
  , fromInt16BE
  , fromInt16LE
  , fromInt32BE
  , fromInt32LE
  , fromString
  , getInt8
  , getInt16BE
  , getInt16LE
  , getInt32BE
  , getInt32LE
  , toString
  ) where

import Prelude

import Data.CatList (CatList(..))
import Data.CatList as List
import Data.CatQueue (CatQueue(..))
import Data.Foldable (all, foldM, foldl, foldr)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (Buffer, BufferValueType(..))
import Node.Buffer (copy, create, fromString, read, size, toString, write, slice) as Buffer
import Node.Encoding (Encoding)

-- | A space-efficient representation of an array of bytes.
newtype ByteString = ByteString (CatList Buffer)
derive newtype instance semigroupByteString :: Semigroup ByteString
derive newtype instance monoidByteString :: Monoid ByteString

instance eqByteString :: Eq ByteString where
  eq (ByteString a) (ByteString b) = foldr go (const false) a b
    where
      go _ _ CatNil = false
      go x fn (CatCons y tail) = runFn2 equalsImpl x y && all fn tail

data ReadResult a
  = Success a ByteString
  | Failure String
derive instance functorResult :: Functor ReadResult

foreign import allocUnsafeImpl :: EffectFn1 Int Buffer
foreign import equalsImpl :: Fn2 Buffer Buffer Boolean

-- | *O(1)* An empty ByteString.
empty :: ByteString
empty = mempty

-- | *O(n)* Retrieve the length of a ByteString.
length :: ByteString -> Int
length (ByteString l) = foldl combine 0 l
  where
    combine acc el = acc + bufferSize el

-- | *O(1)* Create a ByteString from a Buffer. The original Buffer
-- | should not be modified.
unsafeFreeze :: Buffer -> ByteString
unsafeFreeze = ByteString <<< List.singleton

-- | *O(n)* Write the contents of a ByteString into a Buffer.
flush :: ByteString -> Effect Buffer
flush (ByteString CatNil) = Buffer.create 0
flush (ByteString (CatCons buf (CatQueue Nil Nil))) = pure buf
flush bs @ (ByteString l) = do
  buf <- Buffer.create $ length bs
  _ <- foldM process buf l
  pure buf
  where
    process view el = do
      copied <- Buffer.copy 0 (bufferSize el) el 0 view
      pure $ Buffer.slice copied (bufferSize view) view

-- | *O(n)* Flatten the contents of a ByteString so that it only
-- | consists of a single chunk.
compact :: ByteString -> Effect ByteString
compact = map unsafeFreeze <<< flush

-- | *O(n)* Split a ByteString in two parts at a given offset.
splitAt :: Int -> ByteString -> Tuple ByteString ByteString
splitAt n (ByteString l) =
  let Tuple a b = go n l
  in Tuple (ByteString a) (ByteString b)
  where
    go n' l' =
      case List.uncons l' of
        Just (Tuple h t) | bufferSize h >= n' ->
          let a = List.singleton $ Buffer.slice 0 n' h
              b = List.cons (Buffer.slice n' (bufferSize h) h) t
          in Tuple a b
        Just (Tuple h t) ->
          let Tuple a b = go (n' - bufferSize h) t
          in Tuple (List.cons h a) b
        Nothing -> mempty

-- | *O(n)* Take first n bytes of a ByteString.
take :: Int -> ByteString -> ByteString
take n = fst <<< splitAt n

-- | *O(n)* Take first n bytes of a ByteString or fail with an error.
take' :: Int -> ByteString -> ReadResult ByteString
take' n bs =
  case splitAt n bs of
    Tuple h t | length h >= n -> Success h t
    _ -> Failure "Tried to read past the buffer"

-- | *O(n)* Drop first n bytes of a ByteString.
drop :: Int -> ByteString -> ByteString
drop n = snd <<< splitAt n

fromInt8 :: Int -> ByteString
fromInt8 = fromScalar Int8 <<< Int.toNumber

fromInt16BE :: Int -> ByteString
fromInt16BE = fromScalar Int16BE <<< Int.toNumber

fromInt16LE :: Int -> ByteString
fromInt16LE = fromScalar Int16LE <<< Int.toNumber

fromInt32BE :: Int -> ByteString
fromInt32BE = fromScalar Int32BE <<< Int.toNumber

fromInt32LE :: Int -> ByteString
fromInt32LE = fromScalar Int32LE <<< Int.toNumber

fromFloat32BE :: Number -> ByteString
fromFloat32BE = fromScalar FloatBE

fromFloat32LE :: Number -> ByteString
fromFloat32LE = fromScalar FloatLE

fromString :: String -> Encoding -> ByteString
fromString str = unsafeFreeze <<< unsafePerformEffect <<< Buffer.fromString str

fromScalar :: BufferValueType -> Number -> ByteString
fromScalar tpe n = unsafePerformEffect do
  buf <- runEffectFn1 allocUnsafeImpl (sizeOf tpe)
  Buffer.write tpe n 0 buf
  pure $ unsafeFreeze buf

getInt8 :: ByteString -> ReadResult Int
getInt8 = map Int.floor <<< getScalar Int8

getInt16BE :: ByteString -> ReadResult Int
getInt16BE = map Int.floor <<< getScalar Int16BE

getInt16LE :: ByteString -> ReadResult Int
getInt16LE = map Int.floor <<< getScalar Int16LE

getInt32BE :: ByteString -> ReadResult Int
getInt32BE = map Int.floor <<< getScalar Int32BE

getInt32LE :: ByteString -> ReadResult Int
getInt32LE = map Int.floor <<< getScalar Int32LE

getFloat32BE :: ByteString -> ReadResult Number
getFloat32BE = getScalar FloatBE

getFloat32LE :: ByteString -> ReadResult Number
getFloat32LE = getScalar FloatLE

getScalar :: BufferValueType -> ByteString -> ReadResult Number
getScalar tpe bs = unsafePerformEffect do
  let Tuple bs' rem = splitAt required bs
  buf <- flush bs'
  if bufferSize buf < required
  then pure $ Failure "Tried to read past the buffer"
  else do
    val <- Buffer.read tpe 0 buf
    pure $ Success val rem
  where
    required = sizeOf tpe

toString :: ByteString -> Encoding -> String
toString bs enc = unsafePerformEffect $ Buffer.toString enc =<< flush bs

sizeOf :: BufferValueType -> Int
sizeOf Int8 = 1
sizeOf UInt8 = 1
sizeOf UInt16BE = 2
sizeOf Int16BE = 2
sizeOf UInt16LE = 2
sizeOf Int16LE = 2
sizeOf UInt32BE = 4
sizeOf Int32BE = 4
sizeOf UInt32LE = 4
sizeOf Int32LE = 4
sizeOf FloatBE = 4
sizeOf FloatLE = 4
sizeOf DoubleBE = 8
sizeOf DoubleLE = 8

bufferSize :: Buffer -> Int
bufferSize = unsafePerformEffect <<< Buffer.size
