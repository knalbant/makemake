import Data.List
import Data.Char (isSpace)
import System.Environment
import System.FilePath
import System.Directory



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
getHeaders = getincludes . nonnull . cleanup
  where cleanup     = map strip . lines
        nonnull     = filter (not . null)
        getincludes = filter ((==) '#' . head)


--Gets the filenames of local headers 
getLocals :: [String] -> [String]
getLocals = filter (any (=='"'))

getFiles :: String -> [String]
getFiles = getDependencies . getLocals . getHeaders


--the default compilation flags
defaultFlags :: String
defaultFlags = "-ansi -Wall -g"

--getCFiles :: [String] -> [String]
getCFiles = filter (isCppFile . getExtension)  
  where getExtension  = dropWhile (/='.')
        isCppFile ext = ext == ".c" || ext == ".cpp"

{-prints out the line which has the .o:
dotOs :: [String] -> String -> String
dotOs headers fileName =
  where dotOd = fileName ++ ".o"
        headersO = map (++ ".o")  -}

main = do
     
     additionalArgs <- getArgs

     files <- getDirectoryContents "./cFiles"
    
     let cFiles = getCFiles files 


     mapM_ putStrLn cFiles

     mapM_ putStrLn additionalArgs
