main :: IO()
main = do
  print "What's your name?"
  name <- getLine
  print "What's your age?"
  age <- getLine
  print ( greeting name age )

agePart age = "(" ++ age ++ "L)"
namePart name = "Hello " ++ name
greeting name age = namePart name ++ 
                    agePart age

ask :: String -> IO(String)
ask question = do
  print question
  answer <- getLine
  return answer       
