module Codewars.Parentheses where

validParentheses :: String -> Bool
validParentheses = check . foldl process (0, True) . map parse
  where
    parse x = if x == '(' then 1 else -1
    process (_sum, _res) i = (_sum + i, _res && ( _sum + i ) >= 0 )
    check (_sum, _res) = _sum == 0 && _res