{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

module WebLesson3 where

-- base
import qualified Data.List as List

-- bytestring
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Char8      as ASCII
import qualified Data.ByteString.Lazy.Char8 as LASCII
import qualified Data.ByteString.Builder    as BSB

-- network
import           Network.Socket                 (Socket)
import qualified Network.Socket.ByteString      as Socket
import qualified Network.Socket.ByteString.Lazy as LSocket

-- network-simple
import qualified Network.Simple.TCP as NS


--------------------------------------------------------------------------------
--  Lesson 1: Sockets
--------------------------------------------------------------------------------

{- In lesson 1, we introduced sockets and defined this foundation for all of the
servers we're going to write. The argument 'f' is how we specify what our server
will do each time a new client opens a connection. The connection is represented
by a *socket*, to which we can read and write byte strings. -}

server :: (Socket -> IO ()) -> IO a
server f =
    NS.serve NS.HostAny "8000" $ \(socket, _socketAddress) ->
        f socket


--------------------------------------------------------------------------------
--  Lesson 2: Say Hello to RFC 7230
--------------------------------------------------------------------------------

{- In lesson 2, we wrote a function called 'sayHello' which writes an HTTP
response to a socket. When we use this as the argument to the 'server' function
from lesson 1, we get a very basic working web server. -}

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
    ASCII.pack (List.intercalate "\r\n" xs)

sayHello :: Socket -> IO ()
sayHello socket =
    Socket.sendAll socket helloResponse_byteString


--------------------------------------------------------------------------------
--  Lesson 3: HTTP types
--------------------------------------------------------------------------------

-- RFC 7230, section 1.2: Syntax Notation

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
                                                       deriving (Eq, Show,
                                                         Bounded, Enum, Ord)

-- RFC 7230, section 2.6: Protocol Versioning

data HttpVersion = HttpVersion Digit Digit             deriving (Eq, Show)

-- RFC 7230, section 3: Message Format

data Request = Request RequestLine [HeaderField] (Maybe MessageBody)
                                                       deriving (Eq, Show)

data Response = Response StatusLine [HeaderField] (Maybe MessageBody)
                                                       deriving (Eq, Show)

-- RFC 7230, section 3.1.1: Request Line

data RequestLine = RequestLine Method RequestTarget HttpVersion
                                                       deriving (Eq, Show)

newtype Method = Method BS.ByteString                  deriving (Eq, Show)

-- RFC 7230, section 3.1.2: Status Line

data StatusLine = StatusLine HttpVersion StatusCode ReasonPhrase
                                                       deriving (Eq, Show)

data StatusCode = StatusCode Digit Digit Digit         deriving (Eq, Show)

newtype ReasonPhrase = ReasonPhrase BS.ByteString      deriving (Eq, Show)

-- RFC 7230, section 3.2: Header Fields

data HeaderField = HeaderField FieldName FieldValue    deriving (Eq, Show)

newtype FieldName = FieldName BS.ByteString            deriving (Eq, Show)

newtype FieldValue = FieldValue BS.ByteString          deriving (Eq, Show)

-- RFC 7230, section 3.3: Message Body

newtype MessageBody = MessageBody LBS.ByteString       deriving (Eq, Show)

-- RFC 7230, section 5.3: Request Target

newtype RequestTarget = RequestTarget BS.ByteString    deriving (Eq, Show)
