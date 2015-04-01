import Data.List
import Data.Char (isSpace)

--accepts the lines with local header files and returns
--the names of the headers stripped of the .h
getDependencies :: [String] -> [String]
getDependencies = map getDependencies'
  where getDependencies' = takeWhile (/='.') . tail . dropWhile (/='"')


--remove trailing whitespace
--replace with something more efficient later
rStrip :: String -> String
rStrip = reverse . dropWhile isSpace . reverse

--remove leading whiteSpace
lStrip :: String -> String
lStrip = dropWhile isSpace

strip :: String -> String
strip = lStrip . rStrip


--gets all the headers (regardless of whether they're local or not)
getHeaders :: String -> [String]
getHeaders = getincludes . nonull . cleanup
  where cleanup     = map strip . lines
        nonnull     = filter (not . null)
        getincludes = filter ((==)'#' . head)
