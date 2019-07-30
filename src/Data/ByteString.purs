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
  , fromInt32BE
  , fromString
  , getInt8
  , getInt16BE
  , getInt32BE
  , toString
  ) where

import Prelude

import Data.CatList (CatList(..))
import Data.CatList as List
import Data.CatQueue (CatQueue(..))
import Data.Foldable (all, foldM, foldl, foldr)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (Buffer, BufferValueType(..))
import Node.Buffer (copy, create, fromString, read, size, toString, write) as Buffer
import Node.Buffer.Unsafe (slice) as Buffer
import Node.Encoding (Encoding)

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

empty :: ByteString
empty = mempty

length :: ByteString -> Int
length (ByteString l) = foldl combine 0 l
  where
    combine acc el = acc + bufferSize el

unsafeFreeze :: Buffer -> ByteString
unsafeFreeze = ByteString <<< List.singleton

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

compact :: ByteString -> Effect ByteString
compact = map unsafeFreeze <<< flush

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

take :: Int -> ByteString -> ByteString
take n = fst <<< splitAt n

take' :: Int -> ByteString -> ReadResult ByteString
take' n bs =
  case splitAt n bs of
    Tuple h t | length h >= n -> Success h t
    _ -> Failure "Tried to read past the buffer"

drop :: Int -> ByteString -> ByteString
drop n = snd <<< splitAt n

fromInt8 :: Int -> ByteString
fromInt8 = fromScalar Int8

fromInt16BE :: Int -> ByteString
fromInt16BE = fromScalar Int16BE

fromInt32BE :: Int -> ByteString
fromInt32BE = fromScalar Int32BE

fromString :: String -> Encoding -> ByteString
fromString str = unsafeFreeze <<< unsafePerformEffect <<< Buffer.fromString str

fromScalar :: BufferValueType -> Int -> ByteString
fromScalar tpe n = unsafePerformEffect do
  buf <- runEffectFn1 allocUnsafeImpl (sizeOf tpe)
  Buffer.write tpe n 0 buf
  pure $ unsafeFreeze buf

getInt8 :: ByteString -> ReadResult Int
getInt8 = getScalar Int8

getInt16BE :: ByteString -> ReadResult Int
getInt16BE = getScalar Int16BE

getInt32BE :: ByteString -> ReadResult Int
getInt32BE = getScalar Int32BE

getScalar :: BufferValueType -> ByteString -> ReadResult Int
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
