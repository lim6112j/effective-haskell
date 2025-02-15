{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main(main) where
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import qualified Data.ByteString.Char8 as BC
import Data.Word
import System.Posix.Types (FileMode, CMode(..))
import Text.Read (readEither)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Printf
import Data.Bits ((.&.), (.|.), shift)
data FileData = FileData
  { fileName :: FilePath
  , fileSize :: Word32
  , filePermissions :: FileMode
  , fileData :: ByteString
  } deriving (Eq, Read, Show)
newtype FilePack =
  FilePack {getPackedFiles :: [FileData]} deriving (Eq, Read, Show)
packFiles :: FilePack -> ByteString
packFiles =
  B64.encode . BC.pack . show
unpackFiles :: ByteString -> Either String FilePack
unpackFiles serializedData =
  B64.decode serializedData >>= readEither . BC.unpack
sampleFilePack :: FilePack
sampleFilePack = FilePack [FileData "test.txt" 0o644 0o644 "hello world"]
testPackFile :: ByteString
testPackFile = packFiles sampleFilePack
testUnpackFile :: Either String FilePack
testUnpackFile = unpackFiles testPackFile
testRoundTrip :: FilePack -> Bool
testRoundTrip pack =
  (Right pack) == (unpackFiles . packFiles) pack
data FileContents
  = StringFileContents String
  | TextFileContnents Text
  | ByteStringFileContents ByteString
  deriving (Eq, Read, Show)
data FileData' = FileData'
  { fileName' :: FilePath
  , fileSize' :: Word32
  , filePermissions' :: FileMode
  , fileData' :: FileContents
  } deriving (Eq, Read, Show)
newtype FilePack' =
  FilePack' {getPackedFiles' :: [FileData']} deriving (Eq, Read, Show)
sampleFilePack' :: FilePack'
sampleFilePack' =
  FilePack'
  [ FileData' "test.txt" 0o644 0o644 (StringFileContents "hello string")
  , FileData' "textFile" 0 0 $ TextFileContnents "hello text"
  , FileData' "binaryFile" 0 0 $ ByteStringFileContents "hello binary"
  ]
-- serializing with type class
class Encode a where
  encode :: a -> ByteString
instance Encode ByteString where
  encode = id

class Decode a where
  decode :: ByteString -> Either String a
instance Decode ByteString where
        decode = Right
instance Encode Text where
  encode = encodeUtf8
instance Decode Text where
        decode = Right . decodeUtf8
instance Encode String where
  encode = BC.pack
instance Decode String where
        decode = Right . BC.unpack
word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. (shift word (-8))
      c = fromIntegral $ 255 .&. (shift word (-16))
      d = fromIntegral $ 255 .&. (shift word (-24))
  in (a, b, c, d)
printBytes :: (PrintfArg t1, PrintfArg t2, PrintfArg t3, PrintfArg t4,
 PrintfType t5) => (t1, t2, t3, t4) -> t5
printBytes (a, b, c, d) = printf "%02x %02x %02x %02x\n" a b c d

word32ToBytesString :: Word32 -> ByteString
word32ToBytesString word =
  let (a, b, c, d) = word32ToBytes word
  in BS.pack [a, b, c, d]

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a, b, c, d) =
  let
    a' = fromIntegral a
    b' = fromIntegral b
    c' = fromIntegral c
    d' = fromIntegral d
  in a' .|. b' .|. c' .|. d'

consWord32 :: Word32 -> ByteString -> ByteString
consWord32 word bytestring =
  let packedWord = word32ToBytesString word
  in packedWord <> bytestring

bytestringToWord32 :: ByteString -> Either String Word32
bytestringToWord32 bytestring =
  case BS.unpack bytestring of
    [a, b, c, d] -> Right $ word32FromBytes (a, b, c, d)
    _otherwise ->
      let l = show $ BS.length bytestring
      in Left ("Expecting 4 bytes but got " <> l)

instance Encode Word32 where
  encode = word32ToBytesString
instance Decode Word32 where
  decode = bytestringToWord32
instance Encode Word16 where
  encode word =
    let a = fromIntegral $ 255 .&. word
        b = fromIntegral $ 255 .&. (shift word (-8))
    in BS.pack [a, b]

instance Decode Word16 where
        decode bytestring =
          case BS.unpack bytestring of
            [a, b] -> Right $ fromIntegral a .|. (shift (fromIntegral b) 8)
            _otherwise ->
              let l = show $ BS.length bytestring
                in Left ("Expecting 2 bytes but got " <> l)
-- filemode is an alias for CMODE, which is a newtype wrapper arount Word32
instance Encode FileMode where
        encode (CMode mode) = encode mode
instance Decode FileMode where
  decode = fmap CMode . decode

data FileData'' a = FileData''
  { fileName'' :: FilePath
  , fileSize'' :: Word32
  , filePermissions'' :: FileMode
  , fileData'' :: a
  } deriving (Eq, Read, Show)

instance Encode a => Encode (FileData'' a ) where
  encode FileData''{..} =
    encode fileName''
    <> encode fileSize''
    <> encode filePermissions''
    <> encode fileData''

main :: IO ()
main = do
  BS.putStr $ B64.encode "hello world"
  putStrLn "\n"
  BS.putStr $ packFiles $ FilePack [FileData "test.txt" 0o644 0o644 "hello world"]
  putStrLn "\n"
  print $ testRoundTrip sampleFilePack
  print $ encode $ FileData'' @String "testPath" 0 0 "Foo"
