{--
-- EPITECH PROJECT, 2024
-- B-FUN-400-TLS-4-1-compressor-jacques.sapin
-- File description:
-- Main
--}

module Main (main) where

import System.Environment
import System.Exit
import Data.Maybe
import Text.Read
import System.IO
import System.Random
import Data.List
import System.IO.Error (catchIOError)
import Data.Function

data Conf = Conf {
    nRule :: Int,
    lRule :: Float,
    fRule :: String
} | Error {
    error :: String
} deriving (Show)

type MyPoint = (Int, Int)
type MyRGB = (Int, Int, Int)

data Pixel = Pixel MyPoint MyRGB deriving (Show)
data Cluster = Cluster { centroid :: MyRGB, pixels :: [Pixel] } deriving (Show)


data PLIST = PLIST [Pixel] deriving (Show)


instance Eq Cluster where
  (Cluster centroid1 _) == (Cluster centroid2 _) = centroid1 == centroid2


getRGB :: Pixel -> MyRGB
getRGB (Pixel _ rgb) = rgb


isValidFile :: FilePath -> IO Bool
isValidFile filePath = catchIOError(openFile filePath ReadMode 
                        >> return True) (\_ -> return False)

readAndStorePixels :: FilePath -> IO (Either String PLIST)
readAndStorePixels filePath = do
    contents <- readFile filePath
    let linesOfFile = lines contents
        parsedPixels = mapM parseLine linesOfFile
    case parsedPixels of
        Just validPixels -> return $ Right (PLIST validPixels)
        Nothing -> return $ Left "Invalid pixel found."

parseLine :: String -> Maybe Pixel
parseLine line =
 case words line of
   [pointStr, tripleStr] ->
     case(readMaybe pointStr::Maybe MyPoint,readMaybe tripleStr::Maybe MyRGB)of
       (Just point,Just triple)|isValidTriple triple->Just (Pixel point triple)
       _ -> Nothing
   _ -> Nothing


isValidTriple :: (Int, Int, Int) -> Bool
isValidTriple (x, y, z) = all (\n -> n >= 0 && n <= 255) [x, y, z]


argsParser :: Conf -> [String] -> IO Conf
argsParser (Conf _ l f) ("-n":n:ns)
  | isNothing (readMaybe n :: Maybe Int) = return $ Error "-n [x1] Must be Int"
  | (read n :: Int) < 0 = return $ Error "-n [x1] Must be Int > 0"
  | (read n :: Int) > 1677721 = return $ Error "-n [x1] Must be Int < 1677722"
  | otherwise = argsParser (Conf (read n) l f) ns

argsParser (Conf n _ f) ("-l":l:ls)
  | isNothing (readMaybe l :: Maybe Float) =return$Error"-l [x2] Must be Float"
  | (read l :: Float) < 0 = return $ Error "-n [x2] Must be Float > 0"
  | otherwise = argsParser (Conf n (read l) f) ls

argsParser (Conf n l _) ("-f":f:fs) = do
    fileExists <- isValidFile f
    if fileExists
        then argsParser (Conf n l f) fs
        else return $ Error $ "-f " ++ f ++ " is not a valid file"

argsParser conf [] = return conf

argsParser _ (x:_) = return $ Error ( x ++ ": unknown argument")


chooseRandCentroid :: [Pixel] -> Int -> IO [Pixel]
chooseRandCentroid pixelList n = do
    indices <- getRandomIndices (length pixelList) n
    return $ map (pixelList !!) indices

getRandomIndices :: Int -> Int -> IO [Int]
getRandomIndices maxIndex n = do
    gen <- newStdGen
    return $ take n $ nub $ randomRs (0, maxIndex - 1) gen

distance :: MyRGB -> MyRGB -> Float
distance (r1,g1,b1) (r2,g2,b2)=sqrt$fromIntegral((r1-r2)^2+(g1-g2)^2+(b1-b2)^2)


printKmeans :: [Cluster] -> IO ()
printKmeans clusters = mapM_ printCluster clusters

printCluster :: Cluster -> IO ()
printCluster (Cluster centroid p) = 
  putStrLn "--" >>
  putStrLn (show centroid) >>
  putStrLn "-" >>
  mapM_ (\(Pixel(x,y)rgb)->putStrLn$"("++show x++","++show y++") "++show rgb)p


nearestCluster :: Pixel -> [Cluster] -> Cluster
nearestCluster pixel clusters = 
  minimumBy (compare `on` (\c-> distance (getRGB pixel) (centroid c))) clusters


updtClustr :: Pixel -> Cluster -> Cluster
updtClustr pixel (Cluster centroid' ps) = Cluster centroid' (pixel : ps)


fillClusters :: [Pixel] -> [Cluster] -> [Cluster]
fillClusters pixels clusters = foldl' assignPixelToCluster clusters pixels
 where
  assignPixelToCluster :: [Cluster] -> Pixel -> [Cluster]
  assignPixelToCluster cs pixel = 
    map(\c->if centroid c==centroid nearest then updtClustr pixel c else c)cs
    where
      nearest = nearestCluster pixel cs


initClusters :: [Pixel] -> [Cluster]
initClusters = map (\(Pixel point rgb) -> Cluster rgb []) 


recalculateCentroidRGB :: [Cluster] -> [MyRGB]
recalculateCentroidRGB clusters = map calculateMeanRGB clusters
 where
   calculateMeanRGB :: Cluster -> MyRGB
   calculateMeanRGB (Cluster _ p) =
     let (rSum,gSum,bSum) = foldl' (\(rAcc,gAcc,bAcc)(Pixel _ (r,g,b))->(rAcc+r,gAcc+g,bAcc+b))(0,0,0)p
         numPixels = length p
     in (rSum `div` numPixels, gSum `div` numPixels, bSum `div` numPixels)


attribuateCentroid :: [MyRGB] -> [Cluster]
attribuateCentroid centroids = map (\rgb -> Cluster rgb []) centroids


myKMeans :: Float -> [Pixel] -> [Cluster] -> [Cluster]
myKMeans l pixels clusters = loop clusters
  where
    loop :: [Cluster] -> [Cluster]
    loop prevClusters =
      let newCentroids = recalculateCentroidRGB prevClusters
          newClusters = fillClusters pixels $ attribuateCentroid newCentroids
          averageDistance = calculateAverageDistance prevClusters newClusters
      in if averageDistance <= l
            then newClusters
            else loop newClusters

calculateAverageDistance :: [Cluster] -> [Cluster] -> Float
calculateAverageDistance prevClusters newClusters =
    let distances = zipWith (distance `on` centroid) prevClusters newClusters
  in sum distances / fromIntegral (length distances)
 

main :: IO ()
main = do
 args <- getArgs
 let defaultConf = Conf (-1) (-1) ""
 parse <- argsParser defaultConf args
 case parse of
     Error err -> putStrLn err >> exitWith (ExitFailure 84)
     Conf n l f -> do
         pixelsOrError <- readAndStorePixels f
         case pixelsOrError of
             Left errMsg -> putStrLn errMsg >> exitWith (ExitFailure 84)
             Right (PLIST plist) -> do
               centroids <- chooseRandCentroid  plist n
               let clusters = myKMeans l plist $ fillClusters plist $ attribuateCentroid $ recalculateCentroidRGB $ fillClusters plist $ initClusters centroids
               printKmeans clusters
