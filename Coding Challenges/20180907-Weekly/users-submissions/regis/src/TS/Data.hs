{-# LANGUAGE OverloadedStrings #-}

module TS.Data where

import           Control.Exception.Base         ( Exception )
import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Vector                   as V
                                                ( (!) )

type Coord = (Double, Double)
                                                
-- | A point in the 2D euclidian space
data Point = Point { label :: Text
                   , coords :: Coord
                   } deriving (Show)

instance Eq Point where
  a == b = label a == label b

-- | JSON decoder
instance FromJSON Point where
  parseJSON = withObject "Point" $ \obj -> Point
    <$> obj .: "label"
    <*> (obj .: "coordinates" >>= withArray "(x,y)" coord)
    where coord arr = do arr' <- mapM parseJSON arr
                         return (arr' V.! 0, arr' V.! 1)

-- | A map with an ID and a set of points
data Map = Map Text [Point] deriving (Eq, Show)

instance FromJSON Map where
  parseJSON = withObject "Map" $ \obj -> Map <$> obj .: "mapId" <*> obj .: "points"

-- | Challengers compete against each other, there are represented
-- by a path and its associated cost
newtype Challenger = Challenger { getChallenger :: ([Point], Double) } deriving (Eq)

instance Ord Challenger where
  (Challenger (_, c)) <= (Challenger (_, c')) = c <= c'
    
-- | Cost of a travel trajectory
cost :: [Point] -> Double
cost xs = foldl1 (+) (uncurry distance <$> zip xs (tail xs))

-- | Euclidian distance
distance :: Point -> Point -> Double
distance (Point _ (x, y)) (Point _ (x', y')) =
  sqrt $ (x - x') ^ 2 + (y - y') ^ 2

data InvalidJsonException = InvalidJsonException

instance Exception InvalidJsonException

instance Show InvalidJsonException where
  showsPrec _ InvalidJsonException = showString "invalid JSON fetched from server"