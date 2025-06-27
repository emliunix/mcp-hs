{-# LANGUAGE
  OverloadedStrings
#-}
-- | For JSON boundary detection in JSON-RPC stdio transport
-- | basically parse to a JSON value length in bytes
module JsonRpc.NoJSON where

import Control.Applicative
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString as A

w8 :: Char -> Word8
w8 = fromIntegral . fromEnum

pNull :: Parser Int
pNull = B.length <$> string "null" 

pBool :: Parser Int
pBool = ((B.length <$> string "true") <|> (B.length <$> string "false"))

pNumber :: Parser Int
pNumber =
  sum <$> sequenceA
    [ (option 0 (word8 (w8 '-') *> pure 1) <?> "sign")
    , (word8 (w8 '0') *> pure 1 <|> satisfy digits1 *> ((+ 1) . B.length <$> A.takeWhile digits) <?> "int")
    , (option 0 frac <?> "frac")
    , (option 0 exp_ <?> "exp")
    ]
  where
    digits c =  c >= (w8 '0') && c <= (w8 '9')
    digits1 c = c > (w8 '0') && c <= (w8 '9')
    frac = word8 (w8 '.') *> ((+ 1) . B.length <$> A.takeWhile digits)
    exp_ = sum <$> sequenceA
      [ (word8 (w8 'e') <|> word8 (w8 'E')) *> pure 1
      , option 0 ((word8 (w8 '-') <|> word8 (w8 '+')) *> pure 1) 
      , B.length <$> A.takeWhile digits
      ]

-- | only the first byte of a utf-8 char for escaping
pUtf8 :: Parser (Word8, ByteString)
pUtf8 = do
  w <- anyWord8
  bs <- case w of
    w | w < 0x80 -> pure ""
    w | w >= 0x80 && w < 0xc0 -> pure "" -- invalid UTF-8 byte
    w | w >= 0xc0 && w < 0xe0 -> A.take 1
    w | w >= 0xe0 && w < 0xf0 -> A.take 2
    w | w >= 0xf0 && w < 0xf8 -> A.take 3
    _ -> pure "" -- invalid UTF-8 byte
  return (w, bs)

pString :: Parser Int
pString =
  word8 (w8 '"') *>
    ((+ 2) . sum <$> manyTill pChar (word8 (w8 '"')))
  where
    pChar :: Parser Int
    pChar = do
      (w, bs) <- pUtf8 <?> "utf8 char"
      e <- if w == w8 '\\'
        then do
          w' <- anyWord8
          if w' == w8 'u'
            then A.take 4 *> pure 5 <?> "unicode escape" 
            else pure 1
        else pure 0
      return (1 + B.length bs + e)

pWhitespaces :: Parser Int
pWhitespaces = B.length <$> A.takeWhile (\c -> c == w8 ' ' || c == w8 '\t' || c == w8 '\n' || c == w8 '\r')

withWhitespaces :: Parser Int -> Parser Int
withWhitespaces p = (+) <$> pWhitespaces <*> p

pObject :: Parser Int
pObject = do
  _ <- word8 (w8 '{')
  f <- pFields
  b <- withWhitespaces (word8 (w8 '}') *> pure 1)
  return (1 + f + b)
  where 
    pFields = sum <$> many (pField <|> pComma)
    pComma = (+) <$> pWhitespaces <*> (word8 (w8 ',') *> pure 1)
    pField = do
      w <- pWhitespaces
      k <- pKey
      c <- pColon
      v <- pValue_
      return $ w + k + c + v
    pKey = withWhitespaces pString
    pColon = withWhitespaces (word8 (w8 ':') *> pure 1)
    pValue_ = withWhitespaces pValue

pArray :: Parser Int
pArray = sum <$> sequenceA
  [ pWhitespaces
  , word8 (w8 '[') *> pure 1
  , sum <$> many (pValue_ <|> pComma)
  , word8 (w8 ']') *> pure 1
  ]
  where
    pValue_ = withWhitespaces pValue
    pComma = withWhitespaces (word8 (w8 ',') *> pure 1)

pValue :: Parser Int
pValue = pNull <|> pBool <|> pNumber <|> pString <|> pObject <|> pArray

pJSON :: Parser Int
pJSON = do
  w <- pWhitespaces
  v <- pValue
  return $ w + v
