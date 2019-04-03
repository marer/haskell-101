module Gpwh.Lesson24 where
import System.IO

helloFile :: IO ()
helloFile = do
    file <- openFile "../../data/hello.txt" ReadMode 
    line1 <- hGetLine file
    line2 <- hGetLine file
    putStrLn line1
    outputFile <- openFile "../../data/output.txt" WriteMode
    hPutStrLn outputFile line2 
    hClose file
    hClose outputFile
    putStrLn "done!"

samplePath = "../../data/hello.txt"

----

fileCounts :: String -> IO ()
fileCounts path = do
    contents <- readFile path
    let msg = getMsg contents
    appendFile "../../data/stats.txt" msg
    putStr msg
    where
    getMsg input = mconcat [path, " chars: ", show charsC, " words: ", show wordsC, " lines: ", show linesC, "\n"]
        where
            charsC = length input
            wordsC = length $ words input
            linesC = length $ lines input