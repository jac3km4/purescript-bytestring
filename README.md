# purescript-bytestring
This library implements a ByteString data structure that consists of a [catenable list](https://github.com/purescript/purescript-catenable-lists) of chunks.
Due to the nature of catenable lists, appending ByteStrings has complexity of O(1) and doesn't involve any copying.
This allows implementing very efficient encoders and parsers.
