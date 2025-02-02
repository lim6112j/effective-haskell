{-# LANGUAGE OverloadedStrings #-}
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
        
main :: IO ()
main = do
  BS.putStr $ B64.encode "hello world"
  putStrLn "\n"
  BS.putStr $ packFiles $ FilePack [FileData "test.txt" 0o644 0o644 "hello world"]
  putStrLn "\n"
  print $ testRoundTrip sampleFilePack
