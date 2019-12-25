module BookStore where


type CustomerId = Int
type ReviewBody = String
data BookInfo = Book Int String [String] deriving (Show)