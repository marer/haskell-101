{-# LANGUAGE OverloadedStrings #-}
module Gpwh.Lesson23 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

search :: T.Text
search = "i"

sampleInput :: T.Text
sampleInput = "this\nis not\ninput"

myLines :: T.Text -> [T.Text]
myLines text = T.splitOn "\n" text

bgText :: T.Text
bgText  = "धर्म\n\nश्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।\nस्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

dharma :: T.Text
dharma = "धर्म"

highlight :: T.Text -> T.Text -> T.Text
highlight search text = T.intercalate ("{" <> search <>"}") $ T.splitOn search text

myMain :: IO ()
myMain = do
    TIO.putStrLn (highlight dharma bgText)

--

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello ", name, "!"]

myHello :: IO ()
myHello = do
   TIO.putStrLn "Hello! What's your name?"
   name <- TIO.getLine
   let statement = helloPerson name
   TIO.putStrLn statement

--

toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines

myLazySum :: IO ()
myLazySum = do
  userInput <- TLIO.getContents
  let numbers = toInts userInput
  TLIO.putStrLn $ (TL.pack . show . sum) numbers