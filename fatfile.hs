--James Vannordstrand
--Operating Systems

module Main where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.List
import Data.Word
import GHC.Int
import Network.CGI.Protocol
import Numeric
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC

type Name       = BL.ByteString
type Attributes = BL.ByteString
type Hi         = BL.ByteString
type Lo         = BL.ByteString
type Size       = BL.ByteString

type CurrentDir = String
type OpenFiles  = [String]
type LocalDirs  = [(String, ClusterChunk)]
type LocalFiles = [(String, ClusterChunk)]

type ClusterChunk = (Name, Attributes, Hi, Lo, Size)
type ParentDir    = (Name, ClusterChunk)
type FileState    = (CurrentDir, OpenFiles, LocalDirs, LocalFiles)

--FILE--
fatFile :: BL.ByteString
fatFile = unsafePerformIO . BL.readFile . head $ unsafePerformIO getArgs

--Globals
bytsPerSec  = decodeByteString decodeWord 11 2 --Bytes Per Sector
secPerClus  = decodeByteString decodeByte 13 1 --Sectors Per Cluster
rsvdSecCnt  = decodeByteString decodeWord 14 2 --Reserved Sector Count
numFATS     = decodeByteString decodeByte 16 1 --Number of FATS occuping a sector
fATSz32     = decodeByteString decodeDoubleWord 36 4 --Count of sectors occupied by one FAT
rootClus    = decodeByteString decodeDoubleWord 44 4 --Location of Root cluster
totSec      = decodeByteString decodeDoubleWord 32 4 --Total count of sectors in all regions

rootDirClus = concatMap (filter isDirectory . decodeCluster . firstSectorofClusterOffset) $ getChainedClusters rootClus
rootSubDirs = map (("/"++) . filter (/=' ') . getName) rootDirClus  --Root's direct subs

firstDataSector = rsvdSecCnt + fromIntegral numFATS * fromIntegral fATSz32
dataSec         = totSec - fromIntegral firstDataSector               --Total count of data sectors (Not just sectors)
countofClusters = fromIntegral dataSec `quot` secPerClus --Same as DataSec since we have 1 sector per cluster

main :: IO ()
main = mainLoop $ initializeFileState ("", [], [], [])

--Main Loop to be executed : Evaluates user input
mainLoop :: FileState -> IO ()
mainLoop fs = do
  putStr $ "\n" ++ getCurDir fs ++ ":> "
  hFlush stdout
  getLine >>= \input -> if input /= "" then mainLoop' fs (words input) else mainLoop' fs [""]
  where mainLoop' fs command 
          | head command == "info"  = info fs
          | head command == "open"  = open fs (unwords $ tail command)
          | head command == "close" = close fs (unwords $ tail command)
          | head command == "size"  = size fs (unwords $ tail command)
          | head command == "cd"    = cd fs (unwords $ tail command)
          | head command == "ls"    = ls fs (unwords $ tail command)
          | head command == "read"  = parseRead command
          | head command == "quit"  = return () 
          | otherwise               = wrongInput fs
        parseRead command 
          | length command >= 4 = reed fs (command !! 1) (mRead $ command !! 2) (mRead $ command !! 3) 
          | otherwise = messageLoop "ERROR : Not enough arguments. " fs
        mRead s = case maybeRead s of
          Just n  -> n
          Nothing -> 0

--Funtions that Mainloop can evaluate into         
info :: FileState -> IO ()
info fs = do
  putStrLn $ "\n\nBytes Per Sector      : 0x" ++ showHex bytsPerSec "" ++ ", " ++ show bytsPerSec
  putStrLn $ "Sectors Per Cluster   : 0x" ++ showHex secPerClus "" ++ ", " ++ show secPerClus
  putStrLn $ "Reserved Sector Count : 0x" ++ showHex rsvdSecCnt "" ++ ", " ++ show rsvdSecCnt
  putStrLn $ "Number of FATS        : 0x" ++ showHex numFATS "" ++ ", " ++ show numFATS
  putStrLn $ "FATSz32               : 0x" ++ showHex fATSz32 "" ++ ", " ++ show fATSz32 ++ "\n\n"
  mainLoop fs

open :: FileState -> String -> IO ()
open fs file
  | file `elem` getOpenFiles fs = messageLoop "ERROR : File already open." fs
  | file `isWithin` getLocalFiles fs = mainLoop $ addFile file fs
  | file `isWithin` getLocalDirs fs = messageLoop "ERROR : Cannot open a directory." fs
  | otherwise = messageLoop "ERROR : File doesn't exist." fs

close :: FileState -> String -> IO ()
close fs file
  | file `elem` getOpenFiles fs = mainLoop $ closeFile file fs
  | otherwise = messageLoop "ERROR : File isn't open." fs

size :: FileState -> String -> IO ()
size fs file
  | file `isWithin` getLocalFiles fs = messageLoop ("The Size is - " ++ (show . getSize . getWithin file $ getLocalFiles fs) ++ " bytes.") fs 
  | otherwise = messageLoop "ERROR : File doesn't exist." fs

cd :: FileState -> String -> IO ()
cd fs dir 
  | getCurDir fs `elem` rootSubDirs && dir == ".." = mainLoop $ initializeFileState fs
  | dir `isWithin` getLocalDirs fs = cd' (fromIntegral . getHiLo . getWithin dir $ getLocalDirs fs)
  | otherwise = messageLoop "ERROR : Not a directory." fs
  where cd' clusterNumber = mainLoop $ updateFileState clusterNumber dir fs

ls :: FileState -> String -> IO ()
ls fs dir
  | dir == "" = thisDir fs
  | dir `isWithin` getLocalDirs fs = anotherDir fs
  | otherwise = messageLoop "ERROR : Not a directory." fs
  where ls' fs = unwords $ (map fst $ getLocalDirs fs) ++ (map fst $ getLocalFiles fs)
        thisDir fs = messageLoop (ls' fs) fs
        anotherDir fs = do
          let clusterNumber = fromIntegral . getHiLo . getWithin dir $ getLocalDirs fs
          messageLoop (ls' $ updateFileState clusterNumber dir fs) fs

reed :: FileState -> String -> Int -> Int -> IO ()
reed fs file position numBytes
  | file `elem` getOpenFiles fs = reedFile fs position numBytes file (getHiLo $ file `getWithin` getLocalFiles fs)
  | otherwise = messageLoop "ERROR : Cannot read from a file not opened." fs

wrongInput :: FileState -> IO ()
wrongInput = messageLoop "Not a valid input"

----------------------------------------------------------------------------------
 --  _    _      _                   ______                _   _                 
 -- | |  | |    | |                 |  ____|              | | (_)                
 -- | |__| | ___| |_ __   ___ _ __  | |__ _   _ _ __   ___| |_ _  ___  _ __  ___ 
 -- |  __  |/ _ \ | '_ \ / _ \ '__| |  __| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
 -- | |  | |  __/ | |_) |  __/ |    | |  | |_| | | | | (__| |_| | (_) | | | \__ \
 -- |_|  |_|\___|_| .__/ \___|_|    |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
 --               | |                                                            
 --               |_|      
----------------------------------------------------------------------------------
reedFile :: FileState -> Int -> Int -> String -> Int -> IO ()
reedFile fs position numBytes file clusterNumber = do
  let fileContent = BC.concat . map (getCluster . firstSectorofClusterOffset) $ getChainedClusters $ fromIntegral clusterNumber
  if (getSize . getWithin file $ getLocalFiles fs) >= position + numBytes 
    then messageLoop (BC.unpack $ readNumBytes (fromIntegral numBytes) position fileContent) fs
    else messageLoop "ERROR : Can't read passed EoF." fs
  where readNumBytes num offset = runGet (do skip offset; getLazyByteString num)
        getCluster offset = runGet (do skip offset; getLazyByteString 512) fatFile

messageLoop :: String -> FileState -> IO ()
messageLoop message fs = do putStrLn message; mainLoop fs

decodeByteString :: Get a -> Int -> Int64 -> a
decodeByteString decode s b = runGet decode $ runGet (do skip s; getLazyByteString b) fatFile

decodeCluster :: Int -> [ClusterChunk]
decodeCluster offset = map parseChunk . filter removeEmpty . listify 0 $ getCluster offset
  where getCluster offset = runGet (do skip offset; getLazyByteString 512) fatFile
        listify 16 _ = [] 
        listify n clusterChunk = runGet (getLazyByteString 32) clusterChunk : listify (n+1) (BL.drop 32 clusterChunk)
        removeEmpty chunk
          |(read (show $ runGet decodeByte $ runGet (getLazyByteString 1) chunk) :: Int) == 229 = False
          |(read (show $ runGet decodeByte $ runGet (getLazyByteString 1) chunk) :: Int) == 0   = False
          | otherwise = True

parseChunk :: BL.ByteString -> ClusterChunk
parseChunk chunk = (runGet (getLazyByteString 11) chunk, runGet (do skip 11; getLazyByteString 1) chunk, 
                    runGet (do skip 20; getLazyByteString 2) chunk, runGet (do skip 26; getLazyByteString 2) chunk,
                    runGet (do skip 28; getLazyByteString 4) chunk)

--The next 5 Functions are used to parse the 32 byte data entries
getName :: ClusterChunk -> String
getName (name, _, _, _, _) = BC.unpack name

getAttributes :: ClusterChunk -> BL.ByteString
getAttributes (_, attributes, _, _, _) = attributes

getHiLo :: ClusterChunk -> Int
getHiLo (_, _, hi, lo, _) = read (show (runGet decodeWord hi) ++ show (runGet decodeWord lo)) :: Int

getSize :: ClusterChunk -> Int
getSize (_, _, _, _, size) = read (show $ runGet decodeDoubleWord size) :: Int

checkAttribute :: String -> BL.ByteString -> Bool
checkAttribute isWhat attributes
  | isWhat == "isDir" = (toBin (read (show $ runGet decodeByte attributes) :: Int) !! 3) == 1
  | isWhat == "isFile" = (toBin (read (show $ runGet decodeByte attributes) :: Int) !! 3) == 0
  | isWhat == "isLong" =  (sum . drop 4 $ toBin (read (show $ runGet decodeByte attributes) :: Int)) == 4
  | isWhat == "isReadOnly" = (toBin (read (show $ runGet decodeByte attributes) :: Int) !! 7) == 1
  | isWhat == "isHidden" = (toBin (read (show $ runGet decodeByte attributes) :: Int) !! 6) == 1
  | isWhat == "isVolume" = (toBin (read (show $ runGet decodeByte attributes) :: Int) !! 4) == 1
  | otherwise = error "Not a valid attribute check"
  where toBin n = if length (toBin' n) /= 8 then replicate (8 - length (toBin' n)) 0 ++ toBin' n else toBin' n
        toBin' 0 = []
        toBin' n | n `mod` 2 == 1 = toBin' (n `div` 2) ++ [1]
                 | n `mod` 2 == 0 = toBin' (n `div` 2) ++ [0]

decodeDoubleWord :: Get Word32
decodeDoubleWord = isEmpty >>= \empty -> if empty then error "No double word" else getWord32host

decodeWord :: Get Word16
decodeWord = isEmpty >>= \empty -> if empty then error "No word" else getWord16host

decodeByte :: Get Word8
decodeByte = isEmpty >>= \empty -> if empty then error "No byte" else getWord8

--Formula Given in Spec
firstSectorofClusterOffset :: Word32 -> Int
firstSectorofClusterOffset cluster = (512*) $ read (show $ firstDataSector + (fromIntegral cluster - 2)^secPerClus) :: Int          

nextClusterLocation :: Word32 -> Word16
nextClusterLocation cluster = (rsvdSecCnt + fatOffSet (fromIntegral cluster) `quot` bytsPerSec) * 512 + fatOffSet (fromIntegral cluster) `rem` bytsPerSec
  where fatOffSet = (*4)

--Next 4 Functions are for checking data
isWithin :: String -> [(String, ClusterChunk)] -> Bool
isWithin _ [] = False
isWithin element ((name, chunk):xs)
  | name == element = True
  | otherwise       = isWithin element xs

getWithin :: String -> [(String, ClusterChunk)] -> ClusterChunk
getWithin _ [] = error "Isn't within!"
getWithin element ((name, chunk):xs)
  | name == element = chunk
  | otherwise       = getWithin element xs 

isDirectory :: ClusterChunk -> Bool
isDirectory x = (not . checkAttribute "isLong" $ getAttributes x) && 
                (checkAttribute "isDir" $ getAttributes x) 

isFile :: ClusterChunk -> Bool
isFile x = (not . checkAttribute "isLong" $ getAttributes x) &&
           (not . checkAttribute "isDir" $ getAttributes x) && 
           (not . checkAttribute "isVolume" $ getAttributes x)

addPeriod :: [String] -> String
addPeriod [x]    = x
addPeriod (x:xs) = x ++ "." ++ addPeriod xs
-----------------------------------------------------------------------------
 --   _____ _        _         ______                _   _                 
 --  / ____| |      | |       |  ____|              | | (_)                
 -- | (___ | |_ __ _| |_ ___  | |__ _   _ _ __   ___| |_ _  ___  _ __  ___ 
 --  \___ \| __/ _` | __/ _ \ |  __| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
 --  ____) | |_ (_| | |_  __/ | |  | |_| | | | | (__| |_| | (_) | | | \__ \
 -- |_____/ \__\__,_|\__\___| |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
-----------------------------------------------------------------------------

initializeFileState :: FileState -> FileState 
initializeFileState fs = do
  let allChainedClusters = getChainedClusters rootClus
  let directories = concatMap (filter isDirectory . decodeCluster . firstSectorofClusterOffset) allChainedClusters
  let files = concatMap (filter isFile . decodeCluster . firstSectorofClusterOffset) allChainedClusters
  let localDirs = zip (map (filter (/=' ') . getName) directories) directories
  let localFiles = zip (map (addPeriod . words . getName) files) files
  ("", getOpenFiles fs, localDirs, localFiles)

updateFileState :: Word32 -> String -> FileState -> FileState 
updateFileState clusterNumber newDir (oldDir, filesOpen, _, _) = do
  let allChainedClusters = getChainedClusters clusterNumber
  let directories = concatMap (filter isDirectory . decodeCluster . firstSectorofClusterOffset) allChainedClusters
  let files = concatMap (filter isFile . decodeCluster . firstSectorofClusterOffset) allChainedClusters
  let localDirs = zip (map (filter (/=' ') . getName) directories) directories
  let localFiles = zip (map (addPeriod . words . getName) files) files
  if newDir == ".." 
    then (reverse . tail . dropWhile (/='/') $ reverse oldDir, filesOpen, localDirs, localFiles)
    else (oldDir ++ "/" ++ newDir, filesOpen, localDirs, localFiles)

--Returns a list of all the chained clusters of a specified cluster number
getChainedClusters :: Word32 -> [Word32]
getChainedClusters clusterNumber 
  | clusterNumber == 268435448 = []
  | otherwise = clusterNumber : getChainedClusters (decodeByteString decodeDoubleWord (fromIntegral $ nextClusterLocation clusterNumber) 4)

closeFile :: String -> FileState -> FileState
closeFile f (dir, filesOpen, localDirs, localFiles) = (dir, closeFile' filesOpen, localDirs, localFiles)
  where closeFile'   []   = []
        closeFile' (x:xs) = if x /= f && f /= "" then x : closeFile' xs else xs  

addFile :: String -> FileState -> FileState
addFile f (dir, filesOpen, localDirs, localFiles) = (dir, addFile' filesOpen, localDirs, localFiles)
  where addFile'   []   = [f | f /= ""]
        addFile' (x:xs) = if x /= f && f /= "" then x : addFile' xs else x:xs

getOpenFiles :: FileState -> OpenFiles
getOpenFiles (_, openFiles, _, _) = openFiles

getCurDir :: FileState -> CurrentDir
getCurDir (currentDir, _, _, _) = currentDir

getLocalDirs :: FileState -> LocalDirs
getLocalDirs (_, _, localDirs, _) = localDirs

getLocalFiles :: FileState -> LocalFiles
getLocalFiles (_, _, _, localFiles) = localFiles