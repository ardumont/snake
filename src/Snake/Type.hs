module Snake.Type where

import           Control.Lens

-- | Entities have coordinates
type Coord = Point

-- | Apple
data Apple = Apple { _coorda :: Coord }
             deriving (Eq, Show)

-- | Snake eats apple
data Snake = Snake { _direction :: Direction, _coords :: Coord }
             deriving (Eq, Show)

-- | Snake's possible direction
data Direction = DirUp | DirRight | DirDown | DirLeft

-- | World has limits
type Limit = Limit Int Int
