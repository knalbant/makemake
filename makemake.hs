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
getLocals = filter (elem '"') 


--returns the names of the local headers stripped of extension 
getLocalHeaders :: String -> [String]
getLocalHeaders = getDependencies . getLocals . getHeaders


--returns a filename with the extension removed 
removeExtension :: String -> String
removeExtension = takeWhile (/='.') 

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

--returns a string corresponding to the instructions to compile the object files and create the executable in the makefile
executableString executableName additionalFlags files  = 
  executableName ++ ":" ++ " " ++ objectFiles ++ "\n\t" ++ "g++ " ++ defaultFlags ++ " " 
  ++ "-o " ++ executableName ++ " " ++ additionalFlags ++ " " ++ objectFiles ++ "\n\n"   


  where objectFiles = unwords $ map ( (++".o") . removeExtension) files      


objectFileString

--there has gotta be a better way to do this
stringListFromIOList :: [IO String] -> IO [String]
stringListFromIOList [] = return []
stringListFromIOList (x:xs) = do

  unWrapped <- x
  restunWrapped <- stringListFromIOList xs
  return (unWrapped: restunWrapped)

--getFilesAndHeaders :: IO (String, [String])
getFilesAndHeaders = do 
  files <- getDirectoryContents "./cFiles"
  let cFiles = getCFiles files 
  let cFilesx = map ("./cFiles/" ++) cFiles 
  let ioContents = map readFile cFilesx  
  fileContents <- stringListFromIOList ioContents 
  let headers = map getLocalHeaders fileContents
  return $ zip cFiles headers

--takes the string list of command line options and transforms into from suitable to be directly written to makefile
makeFlagString :: [String] -> String 
makeFlagString = unwords . map ( "-" ++) 

main = do
     additionalArgs <- getArgs
     if null additionalArgs
     then do 
      putStrLn "Usage: makemake.sh executable_name"
     else do 
      let executableName  = head additionalArgs
      let additionalFlags = makeFlagString $ tail additionalArgs

      filesAndHeaders <- getFilesAndHeaders 
   

      print filesAndHeaders  
      putStr $ executableString executableName additionalFlags (map fst filesAndHeaders) 

      return ()


