{-# LANGUAGE OverloadedStrings #-}
module Gpwh.Lesson26 (marcMain) where
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

-- MARC to HTML

type Author = T.Text
type Title = T.Text
type Html = T.Text

data Book = Book {
    author :: Author,
    title :: Title
} deriving (Show)

marcMain :: IO ()
marcMain = do
    marcData <- B.readFile "data/tmp/sample.mrc"
    let processed = processRecords 500 marcData
    TIO.writeFile "data/tmp/books.html" processed

-- html formatting

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [
    "<html>\n<head><title>books</title><meta charset='utf-8'/></head>\n<body>\n"
   , booksHtml
   ,"</body>\n</html>"]
  where booksHtml = mconcat $ map bookToHtml books

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "\n</p>\n"]
    where
     titleInTags = mconcat ["<strong>", (title book), "</strong>"]
     authorInTags = mconcat ["<em>", (author book), "</em>"]

-- raw record processing

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString

type FieldText = T.Text

data FieldMetadata = FieldMetadata { 
    tag :: T.Text,
    fieldLength :: Int,
    fieldStart  :: Int 
} deriving Show

leaderLength :: Int
leaderLength = 24

dirEntryLength :: Int
dirEntryLength = 12

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength = rawToInt . B.take 5 

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
    where recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty then [] else next : allRecords rest
    where (next, rest) = nextAndRest marcStream 

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = getRecordLength $ B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
    where 
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty then [] else nextEntry : splitDirectory rest
    where (nextEntry, rest) = B.splitAt dirEntryLength directory

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata tag fieldLength fieldStart
    where 
    tag = E.decodeUtf8 $ B.take 3 entry
    fieldLength = rawToInt $ B.take 4 $ B.drop 3 entry
    fieldStart = rawToInt $ B.drop 7 entry

getFieldMetadata ::  [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = fieldText
    where
    baseAddress = getBaseAddress record
    dataPart = B.drop baseAddress record
    fieldText = E.decodeUtf8 $ B.take (fieldLength fieldMetadata) $ B.drop (fieldStart fieldMetadata) dataPart

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if length results > 0 then Just (head results) else Nothing
    where
    metadata = getFieldMetadata $ splitDirectory $ getDirectory record
    results = filter ((==aTag) . tag) metadata

lookupSubfield :: (Maybe FieldMetadata) -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just fieldMetadata) subfield record = if results == [] then  Nothing else Just (T.drop 1 $ head results) 
    where
    rawField = getTextField record fieldMetadata
    subfields = T.split (==fieldDelimiter) rawField
    results = filter ((==subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
    where entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs = map (\r -> (lookupTitle r, lookupAuthor r)) . allRecords
 
pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(t, a) -> Book { title = fromJust t, author = fromJust a }) onlyJusts
    where onlyJusts = filter (\(t, a) -> isJust t && isJust a) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . (take n) . marcToPairs

-- samples
-- https://archive.org/download/marc_oregon_summit_records/catalog_files/

book1 :: Book
book1 = Book {
    title = "The Conspiracy Against the Human Race"
   ,author = "Ligotti, Thomas"
   }

book2 :: Book
book2 = Book {
    title = "A Short History of Decay"
   ,author = "Cioran, Emil"
   }

book3 :: Book
book3 = Book {
    title = "The Tears of Eros"
   ,author = "Bataille, Georges"
   }

myBooks :: [Book]
myBooks = [book1,book2,book3]