module SwapHeadTail where

swapHeadAndTail :: [Int] -> [Int]
swapHeadAndTail [] = []
swapHeadAndTail [a] = [a]
swapHeadAndTail xs = concat $ map ($ xs ) [ drop ( partLen + middleLen ), take middleLen . drop partLen , take partLen ]
    where
        len = length xs
        partLen = div len 2
        middleLen = mod len 2
