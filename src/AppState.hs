module CandW where

import Data.Map as Map
import System.Random

data Direction = UP | DOWN |LEFT | RIGHT deriving (Eq, Ord)
type Wall = [(Int, Int)]
type Car = [(Int, Int)]
type Walls=[Wall]


cols::Int
rows::Int
cols = 50
rows = 33

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList $ zip [UP,DOWN, LEFT, RIGHT] 
                                        [(0, -1), (0, 1),(-1, 0), (1, 0)]

wasWallsHit::GameState->Bool
wasWallsHit (GameState car (w:ws) a b c d e f)= wasWallHit car w || wasWallsHit (GameState car ws a b c d e f)
wasWallsHit (GameState _ [] _ _ _ _ _ _)=False

wasWallHit::Car->Wall->Bool
wasWallHit car wall=wasWallhit
        where
                wasWallhit = (headX<=wallX2 && headX>=wallX1 &&wallY==headY) ||
                        (headX<=wallX2 && headX>=wallX1 &&wallY==(headY-1))||
                        wallY==(headY-2) && ((wallX1<=headX-1 && headX-1<=wallX2) || (wallX1<=headX && headX<=wallX2) || (wallX1<=headX+1 && headX+1<=wallX2))
                (headX, headY) = head car
                (wallX1,wallY) = head wall
                (wallX2,_) = last wall

move':: (Int,Int)->Car->Car
move' (_,_) []=[]
move' (a,b) ((c,d):xs) =(a+c,b+d): move' (a,b) xs

move :: GameState->GameState
move (GameState car wall a dir aa aaa lifes aaaa)= if wasWallsHit (GameState car wall a dir aa aaa lifes aaaa)
                            then (GameState car wall a dir aa aaa (lifes-1) aaaa)
                            else (GameState newcar wall a dir aa aaa lifes aaaa)
    where   newcar = move' (directionVectorMap ! dir) car

gennewWall::Int->Int->Int->Int->Wall
gennewWall x y rn w=[((min (max (x+rn+i) 0) (min (x+rn+i) cols)),(y-1))|i<-[0..w]]

gennewWalls::[Int]->Int->[Int]->[Int]->Walls
gennewWalls (x:xs) y (r:rs) (w:ws)=gennewWall x y r w:gennewWalls xs y rs ws
gennewWalls [] _ _ _ =[]
gennewWalls _ _ [] _ =[]
gennewWalls _ _ _ [] =[]

getHeads::Walls->[Int]
getHeads (w:ws)=(getHead w):(getHeads ws)
getHeads []=[]

getHead::Wall->Int
getHead ((a,_):_) =a
getHead [] =0

moveWalls:: GameState-> (Walls,StdGen,Bool)
moveWalls (GameState _ walls _ _ _ ran _ _)= (gennewWalls wallXs wallY' rns widths,ran',newPoint)
        where
                (_,wallY)=head (head walls)
                wallXs=getHeads walls
                (wallY',rm)
                        |wallY<2 = (rows-2,10)
                        |otherwise =(wallY,1)
                newPoint
                        |rm==1=False 
                        |otherwise =True
                (_,ran')=randomR (-1*rm, 1*rm) ran
                rns=getRandoms ran walls rm
                widths=getWidths ran walls wallY

getRandoms::StdGen->Walls->Int->[Int]
getRandoms ran (_:ws) rm=rn:(getRandoms ran2 ws rm)
        where (rn,ran2)=randomR (-1*rm, 1*rm) ran
getRandoms _ [] _=[]

getWidths::StdGen->Walls->Int->[Int]
getWidths ran (w:ws) y=width:(getWidths ran2 ws y)
        where (width,ran2)
                        |y<2=randomR (1,cols `div` 2) ran
                        |otherwise=(length w-1, ran)
getWidths _ [] _=[]

getn::Int->[a]->a
getn n xs = last (Prelude.take n xs)

checkGameOver :: GameState -> Bool
checkGameOver (GameState car wall point dir over ran lifes record) =   headX == 2 || headX == (cols-2) || 
                      headY == (rows-1)||headY==3||
                      (wasWallsHit (GameState car wall point dir over ran lifes record) && lifes<1)
    where
            (headX, headY) = head car

data GameState = GameState      { getCar :: Car
                                , getWalls :: Walls
                                , getPoints :: Int
                                , getDirection :: Direction
                                , isGameOver :: Bool
                                , getRandomStdGen :: StdGen
                                , getLifes:: Int
                                , getRecord::Int }

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState c w p _ g r l re) newDir = GameState c w p newDir g r l re

initialGameState :: Bool->Int -> GameState
initialGameState gameOver record= GameState   { getCar = [  (carX, carY), 
                                                        (carX, carY - 1),
                                                        (carX, carY - 2), 
                                                        (carX - 1, carY - 2), 
                                                        (carX + 1, carY - 2)]
                                        , getWalls = [[(carX-2,rows-1),(carX-1,rows-1),(carX,rows-1),(carX+1,rows-1), (carX+2,rows-1)],[(cols,rows-1),(cols-1,rows-1),(cols-2,rows-1)],[(1,rows-1),(2,rows-1),(3,rows-1)]]
                                        , getPoints =0
                                        , getDirection = LEFT
                                        , isGameOver = gameOver
                                        , getRandomStdGen = mkStdGen 100 
                                        , getLifes=9
                                        , getRecord=record}
        where   carX = cols `div` 2
                carY = 4
