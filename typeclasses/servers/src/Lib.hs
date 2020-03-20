{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Char                        as Char
import qualified Data.List                        as List

import           Control.Concurrent               (threadDelay)
import           Network.Simple.TCP               as NS
import           Network.Socket                   (Socket)
import qualified Network.Socket.ByteString        as Socket
import qualified Network.Socket.ByteString.Lazy   as LSocket
import           Numeric                          (showHex, showInt)

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as ASCII
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Lazy.Char8       as LASCII

import qualified Data.ByteString.Builder          as BSB
import           Data.Functor.Contravariant       (Equivalence (..), contramap,
                                                   defaultEquivalence)

import           Control.Applicative              (many, optional, (<|>))
import           Data.Attoparsec.ByteString.Char8 (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Maybe                       as Maybe
import           Numeric.Natural                  (Natural)

server :: (Socket -> IO ()) -> IO ()
server f =
    NS.serve NS.HostAny "8000" $
        \(socket, _socketAddress) -> f socket

someFunc :: IO ()
someFunc = putStrLn "someFunc"

helloResponse_byteString :: BS.ByteString
helloResponse_byteString =
    asciiLines
        [ "HTTP/1.1 200 OK"
        , "Content-Type: text/plain; charset=us-ascii"
        , "Content-Length: 7"
        , ""
        , "Hello!\n"
        ]

asciiLines :: [String] -> BS.ByteString
asciiLines xs =
    ASCII.pack (List.intercalate "\r\n" xs)  -- "\r\n" is the CRLF

sayHello :: Socket -> IO ()
sayHello socket =
    Socket.sendAll socket helloResponse_byteString


-------

-- Request

data Request
    = Request RequestLine [HeaderField] (Maybe MessageBody)

data RequestLine
    = RequestLine Method RequestTarget HttpVersion

newtype Method = Method BS.ByteString

newtype RequestTarget = RequestTarget BS.ByteString


-- Response

data Response
    = Response StatusLine [HeaderField] (Maybe MessageBody)

data StatusLine
    = StatusLine HttpVersion StatusCode ReasonPhrase

data StatusCode
    = StatusCode Digit Digit Digit

newtype ReasonPhrase = ReasonPhrase BS.ByteString


-- Header Fields

data HeaderField = HeaderField FieldName FieldValue

newtype FieldName = FieldName BS.ByteString

newtype FieldValue = FieldValue BS.ByteString


-- Body

newtype MessageBody = MessageBody LBS.ByteString


-- HTTP Version

data HttpVersion = HttpVersion Digit Digit


-- Digit

data Digit
    = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9


-- Response Encoders -- when a server sends a response to a Request

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine headerFields bodyMaybe) =
    encodeStatusLine statusLine
    <> encodeHeaderFieldList headerFields
    <> BSB.string7 "\r\n"
    <> foldMap encodeMessageBody bodyMaybe

encodeHeaderFieldList :: [HeaderField] -> BSB.Builder
encodeHeaderFieldList headerFields =
    foldMap
        (\hf -> encodeHeaderField hf <> BSB.string7 "\r\n")
        headerFields

encodeHeaderField :: HeaderField -> BSB.Builder
encodeHeaderField (HeaderField (FieldName x) (FieldValue y)) =
    BSB.byteString x
    <> BSB.string7 ": "
    <> BSB.byteString y

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine httpVersion statusCode reasonPhrase) =
    encodeHttpVersion httpVersion
    <> BSB.string7 " "
    <> encodeStatusCode statusCode
    <> BSB.string7 " "
    <> encodeReasonPhrase reasonPhrase
    <> BSB.string7 "\r\n"

encodeHttpVersion :: HttpVersion -> BSB.Builder
encodeHttpVersion (HttpVersion x y) =
    BSB.string7 "HTTP/"
    <> encodeDigit x
    <> BSB.string7 "."
    <> encodeDigit y

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) =
    encodeDigit x <> encodeDigit y <> encodeDigit z

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase x) = BSB.byteString x

encodeDigit :: Digit -> BSB.Builder
encodeDigit d = BSB.string7 [digitChar d]

encodeMessageBody :: MessageBody -> BSB.Builder
encodeMessageBody (MessageBody x) =
    BSB.lazyByteString x


digitChar :: Digit -> Char
digitChar d =
    case d of
        D0 -> '0'
        D1 -> '1'
        D2 -> '2'
        D3 -> '3'
        D4 -> '4'
        D5 -> '5'
        D6 -> '6'
        D7 -> '7'
        D8 -> '8'
        D9 -> '9'


-------

staticResponseServer :: Response -> IO ()
staticResponseServer response =
    server $ \socket ->
        LSocket.sendAll socket
        (BSB.toLazyByteString (encodeResponse response))

helloResponse_withMoreTypes :: Response
helloResponse_withMoreTypes =
    Response statusLine hfs body
      where
        statusLine = StatusLine
            (HttpVersion D1 D1)
            (StatusCode D2 D0 D0)
            (ReasonPhrase $ ASCII.pack "OK")
        hfs =
            [ HeaderField
                (FieldName $ ASCII.pack "Content-Type")
                (FieldValue $ ASCII.pack "text/plain; charset=us-ascii")
            , HeaderField
                (FieldName $ ASCII.pack "Content-Length")
                (FieldValue $ ASCII.pack "7")
            ]
        body = Just $ MessageBody $ LASCII.pack "Hello!\n"


----

http_1_1 :: HttpVersion
http_1_1 = HttpVersion D1 D1

status200 :: StatusCode
status200 = StatusCode D2 D0 D0

reasonOk :: ReasonPhrase
reasonOk = ReasonPhrase (ASCII.pack "OK")

contentTypeHeader :: String -> HeaderField
contentTypeHeader contentType =
    HeaderField
        (FieldName (ASCII.pack "Content-Type"))
        (FieldValue (ASCII.pack contentType))

plainTextAsciiHeader :: HeaderField
plainTextAsciiHeader = contentTypeHeader "text/plain; charset=us-ascii"

contentLengthHeader :: Integral a => a -> HeaderField
contentLengthHeader contentLength =
    HeaderField
        (FieldName $ ASCII.pack "Content-Length")
        (FieldValue $ ASCII.pack (showInt contentLength ""))

asciiMessageBody :: String -> MessageBody
asciiMessageBody x =
    MessageBody (LASCII.pack x)


---- Using the functions defined above to re-write the response

helloResponse_convenient :: Response
helloResponse_convenient =
    Response
        (StatusLine http_1_1 status200 reasonOk)
        [ plainTextAsciiHeader
        , contentLengthHeader (LASCII.length $ LASCII.pack body)
        ]
        (Just $ asciiMessageBody body)
    where body = "Hello!\n"

-- Header Fields manipulation

alterHeader :: (Maybe FieldValue -> Maybe FieldValue)
            -> FieldName
            -> [HeaderField]
            -> [HeaderField]
alterHeader f fieldName hfs =
    case hfs of
        [] ->
            case (f Nothing) of
                Nothing     -> []
                Just newVal -> [HeaderField fieldName newVal]
        (HeaderField exName exVal : xs) | exName ~= fieldName ->
            case (f (Just exVal)) of
                Nothing     -> xs
                Just newVal -> HeaderField fieldName newVal : xs
        (xy : xys) ->
            xy : alterHeader f fieldName xys
    where
        (~=) :: FieldName -> FieldName -> Bool
        a ~= b = getEquivalence fieldNameEquivalence a b

fieldNameEquivalence :: Equivalence FieldName
fieldNameEquivalence =
    contramap f (defaultEquivalence @String)
    -- above, f is applied first, its result is fed to defaultEquivalence
    where
        f :: FieldName -> String
        f (FieldName x) = Char.toLower <$> ASCII.unpack x

setContentLengthHeader :: Response -> Response
setContentLengthHeader (Response statusLine hfs bodyMaybe) =
    Response statusLine hfs' bodyMaybe
    where
        hfs' = alterHeader f name hfs
        name = FieldName (ASCII.pack "Content-Length")
        f :: Maybe FieldValue -> Maybe FieldValue
        f _ =
            case bodyMaybe of
                Nothing -> Nothing
                Just (MessageBody lbs) ->
                    Just (FieldValue (ASCII.pack (showInt (LBS.length lbs) "")))

-- response without the content-length header
helloResponse_base :: Response
helloResponse_base =
    Response
        (StatusLine http_1_1 status200 reasonOk)
        [plainTextAsciiHeader]
        (Just $ asciiMessageBody "Hello!\n")

helloResponse_withContentLength :: Response
helloResponse_withContentLength =
    setContentLengthHeader helloResponse_convenient


-- utility functions for easily printing a response

showResponseASCII :: Response -> String
showResponseASCII response =
    LASCII.unpack $ BSB.toLazyByteString (encodeResponse response)

printResponseASCII :: Response -> IO ()
printResponseASCII response =
    putStrLn $ showResponseASCII response


-- Chunking the body

helloResponse_bytestring_chunked :: BS.ByteString
helloResponse_bytestring_chunked =
    asciiLines
        [ "HTTP/1.1 200 OK"
        , "Content-Type: text/plain; charset=us-ascii"
        , "Transfer-Encoding: chunked"
        , ""
        , "7"
        , "Hello!\n"
        , "1e"
        , "It is a lovely day today, no?\n"
        , "0\r\n\r\n"
        ]

sayHello_chunked :: Socket -> IO ()
sayHello_chunked socket =
    Socket.sendAll socket helloResponse_bytestring_chunked


-- chunkResponse -> will convert a Response that doesn’t convey its body size into one that conveys its body size via chunking.

chunkResponse :: Response -> Response
chunkResponse response@(Response statusLine hfs bodyMaybe) =
    case bodyMaybe of
        Nothing ->
            response
        Just messageBody ->
            Response
                statusLine
                (setChunkHeader hfs)
                (Just $ chunkMessageBody messageBody)

setChunkHeader :: [HeaderField] -> [HeaderField]
setChunkHeader hfs = alterHeader f name hfs
    where
        name = FieldName $ ASCII.pack "Transfer-Encoding"
        f :: Maybe FieldValue -> Maybe FieldValue
        f oldValueMaybe =
            case oldValueMaybe of
                Nothing ->
                    Just $ FieldValue (ASCII.pack "chunked")
                Just (FieldValue x) ->
                    Just $ FieldValue (x <> ASCII.pack ", chunked")

chunkMessageBody :: MessageBody -> MessageBody
chunkMessageBody (MessageBody lbs) =
    MessageBody (BSB.toLazyByteString (encodeChunkedBody $ LBS.toChunks lbs))

encodeChunkedBody :: [BS.ByteString] -> BSB.Builder
encodeChunkedBody chunks =
    foldMap encodeChunk chunks
    <> encodeLastChunk
    <> BSB.string7 "\r\n"

encodeLastChunk :: BSB.Builder
encodeLastChunk = BSB.string7 "0\r\n"

encodeChunk :: BS.ByteString -> BSB.Builder
encodeChunk bs =
    BSB.string7 (showHex (BS.length bs) "")
    <> BSB.string7 "\r\n"
    <> BSB.byteString bs
    <> BSB.string7 "\r\n"


helloResponse_chunked :: Response
helloResponse_chunked =
    chunkResponse helloResponse_base


--  SLowing it down - chunking and then sending one byte at a time

staticResponseServer_slow :: Response -> IO ()
staticResponseServer_slow response =
    server $ \socket ->
        sendAll_slow socket
            (BSB.toLazyByteString $ encodeResponse response)


sendAll_slow :: Socket -> LBS.ByteString -> IO ()
sendAll_slow socket lbs =
    do
        let (x, y) = LBS.splitAt 1 lbs
        LSocket.sendAll socket x
        if (LBS.null y)
            then return ()
            else do
                threadDelay 5000
                sendAll_slow socket y

infiniteResponse :: Response
infiniteResponse =
    chunkResponse $
        Response
            (StatusLine http_1_1 status200 reasonOk)
            [plainTextAsciiHeader]
            (Just $ infiniteMessageBody)


infiniteMessageBody :: MessageBody
infiniteMessageBody =
    MessageBody (BSB.toLazyByteString $ foldMap line [1..])
      where
          line :: Integer -> BSB.Builder
          line i = BSB.string7 (showInt i "") <> BSB.char7 '\n'


---- Parsing Requests ----


requestParser :: Parser Request
requestParse =
    do
        requestLine <- requestLineParser
        hfs <- headerFieldListParser
        _ <- crlfParser
        bodyMaybe <- messageBodyParser hfs
        return (Request requestLine hfs bodyMaybe)
    <?> "HTTP-message"


requestLineParser :: Parser RequestLine
requestLineParser =
    do
        method <- methodParser
        _ <- spParser
        requestTarget <- requestTargetParser
        _ <- spParser
        httpVersion <- httpVersionParser
        _ <- crlfParser
        return $ RequestLine method requestTarget httpVersion
    <?> "request-line"


methodParser :: Parser Method
methodParser =
    do
        x <- tokenParser
        return (Method x)
    <?> "method"


requestTargetParser :: Parser RequestTarget
requestTargetParser =
    do
        x <- P.takeWhile1 (/= ' ')
        return (RequestTarget x)
    <?> "request-target"


httpVersionParser :: Parser HttpVersion
httpVersionParser =
    do
        _ <- httpNameParser
        _ <- P.char '/'
        x <- digitParser
        _ <- P.char '.'
        y <- digitParser
        return $ HttpVersion y
    <?> "Http-version"


httpNameParser :: Parser ()
httpNameParser =
    do
        _ <- P.string (ASCII.pack "HTTP")
        return ()
    <?> "HTTP-name"


headerFieldListParser :: Parser [HeaderField]
headerFieldListParser =
    many $
        do
            x <- headerFieldParser
            _ <- crlfParser
            return x

headerFieldParser :: Parser HeaderField
headerFieldParser =
    do
        fieldName <- fieldNameParser
        _ <- P.char ':'
        _ <- owsParser
        fieldValue <- fieldValueParser
        _ <- owsParser
        return $ HeaderField fieldName fieldValue
    <?> "header-field"


fieldNameParser :: Parser FieldName
fieldNameParser =
    do
        x <- tokenParser
        return $ FieldName x
    <$> "field-name"


fieldValueParser :: Parser FieldValue
fieldValueParser =
    do
        (x, _) <- P.match $
            P.sepBy
                (P.takeWhile1 isVisiblechar) -- sequence of visible characters
                (P.takeWhile1 $ (P.inClass " \t"))  -- separated by whitespace
        return $ FieldValue x
    <?> "field-value"


-- Miscellaneous functions

crlfParser :: Parser ()
crlfParser =
    do
        _ <- P.string (ASCII.pack "\r\n")
        return ()
    <?> "CRLF"

spParser :: Parser ()
spParser =
    do
        _ <- P.char ' '
        return ()
    <?> "SP"

-- all "visible" characters (any char other than whitespace or control chars), in ASCII are between '!' and '~', so

isVisiblechar :: Char -> Bool
isVisiblechar c =
    c >= '!' && c <= '~'


digitParser :: Parser Digit
digitParser =
    do
        x <- P.satisfy P.isDigit
        case (charDigit x) of
            Just d  -> return d
            Nothing -> fail "Must be between 0 and 9"
    <?> "DIGIT"


charDigit :: Char -> Maybe Digit
charDigit c =
    case x of
        '0' -> Just D0
        '1' -> Just D1
        '2' -> Just D2
        '3' -> Just D3
        '4' -> Just D4
        '5' -> Just D5
        '6' -> Just D6
        '7' -> Just D7
        '8' -> Just D8
        '9' -> Just D9
        _   -> Nothing

isWhiteSpace :: Char -> Bool
isWhiteSpace = P.inClass " \t"

owsParser :: Parser ()
owsParser =
    do
        _ <- P.takeWhile isWhiteSpace
        return ()
    <?> "OWS"

tokenParser :: Parser BS.ByteString
tokenParser =
    P.takeWhile1 isTchar
    <?> "token"

isTchar :: Char -> Bool
isTchar c =
    P.isDigit c ||
    P.isAlpha_ascii c ||
    P.inClass "!#$%&'*+-.^|~" c


---- Message body Parser ---

messageBodyParser :: [HeaderField] -> Parser (Maybe MessageBody)
messageBodyParser =
    parserForBodyEncoding . discernBodyEncoding


discernBodyEncoding :: [HeaderField] -> Either String (Maybe BodyEncoding)
discernBodyEncoding hfs =
    let
        allResults :: Either String [BodyEncoding]
        allResults =
            -- fmap catMaybes (traverse headerFieldBodyEncoding hfs)
            -- or, using do
            do
                -- traverse's sign here is
                -- :: (HeaderField -> Either String (Maybe BodyEncoding))
                -- -> [HeaderField]
                -- -> Either String [Maybe BodyEncoding]
                maybes <- traverse headerFieldBodyEncoding hfs
                -- catMaybes removes Nothing values from [Maybe]
                return (Maybe.catMaybes maybes)
    in
        case allResults of
            Right [] -> Right Nothing
            Right [x] -> Right $ Just x
            -- below: invalid case since it means multiple headers of same field
            Right (_: _) -> Left "Cannot determine body encoding; \
                                 \ there are too many relevant headers"
            Left err -> Left ("Cannot determine body encoding \
                             \ due to invalid headers: " ++ err)



headerFieldBodyEncoding :: HeaderField -> Either String (Maybe BodyEncoding)
headerFieldBodyEncoding (HeaderField name value)
    | nameIs "Content-Length" =
        do
            len <- parseFieldValue decimalParser value
            return (Just (ContentLength len))
    | nameIs "Transfer-Encoding" =
        do
            transferEncodings <- parseFieldValue transferEncodingParser value
            let isChunked = isChunkedTransferCoding (NE.last transferCodings)
            return (if isChunked then (Just Chunked) else Nothing)

    | otherwise =
        return Nothing
    where
        nameIs x =
            getEquivalence fieldNameEquivalence name (FieldName $ ASCII.pack x)

parserForBodyEncoding :: _ -> Parser (Maybe MessageBody)



-- using Natural here because content length cannot be negative

data BodyEncoding
    = ContentLength Natural | Chunked


-- So our parseFieldValue function will:
    -- Apply (<* P.endOfInput) to the parser to make sure that we’re parsing the entire field value;
    -- Use parseOnly to apply the resulting parser to the FieldValue’s ByteString.
parseFieldValue :: Parser a -> FieldValue -> Either String a
parseFieldValue p (FieldValue value) =
    P.parseOnly (p <* P.endOfInput) value


decimalParser :: Parser Natural
decimalParser =
    P.decimal <?> "1*DIGIT"


listParser :: Parser (NonEmpty BS.ByteString)
listParser =
    do
        x <- -- ??
        xs <- many $
            do
                -- ??
        return (x :| xs)

listElementParser :: Parser BS.ByteString
listElementParser =
    -- ??

unqoutedElementParser :: Parser BS.ByteString
unqoutedElementParser =
    P.takeWhile -- ??


quotedElementParser :: Parser BS.ByteString
quotedElementParser =
    do
        _ <- P.string (ASCII.pack "\"")


transferEncodingParser :: Parser (NonEmpty TransferCoding)
