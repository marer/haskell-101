{-# LANGUAGE OverloadedStrings #-}
module Gpwh.Lesson25 where
import System.Environment
import System.Random (randomRIO)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

bcInt :: BC.ByteString
bcInt = "asdasdsasdasadsda"

bcIntToInt :: BC.ByteString -> Int
bcIntToInt = read . BC.unpack

-- glitcher

glitcher :: IO()
glitcher = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile $ "data/" <> fileName
  glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["data/tmp/glitched_",fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = mconcat $ replicate 3 [randomSortSection, randomReplaceByte]

intToChar :: Int -> Char
intToChar int = toEnum $ mod int 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
    where
    newChar = intToBC charVal
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    loc <- randomRIO (1, bytesLength)
    charVal <- randomRIO (0, 255)
    return (replaceByte loc charVal bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,changed,after]
  where (before,rest) = BC.splitAt start bytes
        (target,after) = BC.splitAt size rest
        changed =  BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0,bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

  --- utf8

nagarjunaBC :: BC.ByteString
nagarjunaBC = "Nāgārjuna"

nagarjunaText :: T.Text
nagarjunaText = "Nāgārjuna"

nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText