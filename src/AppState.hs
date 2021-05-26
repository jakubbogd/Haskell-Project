module CandW where

import Data.Map as Map
import System.Random
import Control.Monad.State.Strict

data Direction = UP | DOWN |LEFT | RIGHT deriving (Eq, Ord)
type Wall = [(Int, Int)]
type Car = [(Int, Int)]

type St= StateT Direction IO

cols::Int
rows::Int
cols = 50
rows = 33

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList $ zip [UP,DOWN, LEFT, RIGHT] 
                                        [(0, -1), (0, 1),(-1, 0), (1, 0)]

wasWallHit::GameState->Bool
wasWallHit (GameState car wall _ _ _ _ _ _)=wasWallhit
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
move (GameState car wall a dir aa aaa lifes aaaa)= if wasWallHit (GameState car wall a dir aa aaa lifes aaaa)
                            then (GameState car wall a dir aa aaa (lifes-1) aaaa)
                            else (GameState newcar wall a dir aa aaa lifes aaaa)
    where   newcar = move' (directionVectorMap ! dir) car

gennewWall::Int->Int->Int->Int->Wall
gennewWall x y rn w=[((min (max (x+rn+i) 0) (min (x+rn+i) cols)),(y-1))|i<-[0..w]]

moveWall:: GameState-> (Wall,StdGen,Bool)
moveWall (GameState _ wall _ _ _ ran _ _)= (gennewWall wallX wallY' rn width,ran2,newPoint)
        where
                wallY'
                        |wallY<2 = rows-2
                        |otherwise =wallY
                (wallX,wallY)=head wall
                rm
                        |wallY<2 = 10
                        |otherwise =1
                newPoint
                        |rm==1=False 
                        |otherwise =True
                (rn,ran2) = randomR (-1*rm, 1*rm) ran
                (width,_)
                        |wallY<2=randomR (1,cols `div` 2) ran
                        |otherwise=(length wall-1, ran)

checkGameOver :: GameState -> Bool
checkGameOver (GameState car wall point dir over ran lifes record) =   headX == 2 || headX == (cols-2) || 
                      headY == (rows-1)||headY==3||
                      (wasWallHit (GameState car wall point dir over ran lifes record) && lifes<1)
    where
            (headX, headY) = head car

data GameState = GameState      { getCar :: Car
                                , getWall :: Wall
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
                                        , getWall = [(carX-2,rows-1),(carX-1,rows-1),(carX,rows-1),(carX+1,rows-1), (carX+2,rows-1)]
                                        , getPoints =0
                                        , getDirection = LEFT
                                        , isGameOver = gameOver
                                        , getRandomStdGen = mkStdGen 100 
                                        , getLifes=9
                                        , getRecord=record}
        where   carX = cols `div` 2
                carY = 4


readInts :: String -> Int
readInts x = read x
