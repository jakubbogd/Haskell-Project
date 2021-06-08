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
rows = 50

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList $ zip [UP,DOWN, LEFT, RIGHT] 
                                        [(0, -1), (0, 1),(-1, 0), (1, 0)]

wasWallsHit::GameState->Bool
wasWallsHit (GameState car (w:ws) a b c d e f g)= wasWallHit car w || wasWallsHit (GameState car ws a b c d e f g)
wasWallsHit (GameState _ [] _ _ _ _ _ _ _)=False

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
move (GameState car wall a dir aa aaa aaaaa lifes aaaa)= if wasWallsHit (GameState car wall a dir aa aaa aaaaa lifes aaaa)
                            then (GameState car wall a dir aa aaa aaaaa (lifes-1) aaaa)
                            else (GameState newcar wall a dir aa aaa aaaaa lifes aaaa)
    where   newcar = move' (directionVectorMap ! dir) car

gennewWall::Int->Int->Int->Int->Wall
gennewWall x y rn w=[((((min (max (x+rn+i) 0) (min (x+rn+i) cols))+(max (max (x+rn+i) 0) (min (x+rn+i) cols))) `div` 2),y)|i<-[0..w]]

gennewWalls::[Int]->[Int]->[Int]->[Int]->Walls
gennewWalls (x:xs) (y:ys) (r:rs) (w:ws)=gennewWall x y r w:gennewWalls xs ys rs ws
gennewWalls [] _ _ _ =[]
gennewWalls _ [] _ _ =[]
gennewWalls _ _ [] _ =[]
gennewWalls _ _ _ [] =[]

genWalls::Int->StdGen->Walls
genWalls y ran=[gennewWall ((cols `div` 3)+(i*2*width)) (y-j) 0  width|i<-takeThree 1 2,j<-[0,5,10,15,20]]
        where (width,_)=randomR (2,5) ran

getHeads::Walls->[Int]
getHeads (w:ws)=(getHead w):(getHeads ws)
getHeads []=[]

getHead::Wall->Int
getHead ((a,_):_) =a
getHead [] =0

moveWalls:: GameState-> (Walls,StdGen,StdGen,Bool)
moveWalls (GameState _ walls _ _ _ ran _ _ _)= (gennewWalls wallXs wallYs rns widths,ran',ran'',newPoint)
        where
                (_,wallY)=head (head walls)
                wallXs=getHeads walls
                (wallY',rm)
                        |wallY==rows = (1,7)
                        |otherwise =(wallY,1)
                wallYs=getWallYs walls (wallY-wallY')
                newPoint
                        |rm==1=False 
                        |otherwise =True
                (_,ran')=randomR (-1*rm, 1*rm) ran
                (_,ran'')=randomR (-1*rm, 1*rm) ran'
                rns=getRandoms ran walls rm
                widths=getWidths ran walls wallY

getWallYs::Walls->Int->[Int]
getWallYs (w:ws) diff=(wy+1):(getWallYs ws diff)
        where   wy=y-diff
                (_,y)=head w
getWallYs [] _=[]

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
checkGameOver (GameState car wall point dir over ran ran2 lifes record) =   headX == 2 || headX == (cols-2) || 
                      headY == (rows-19)||headY==3||
                      (wasWallsHit (GameState car wall point dir over ran ran2 lifes record) && lifes<2)
    where
            (headX, headY) = head car

data GameState = GameState      { getCar :: Car
                                , getWalls :: Walls
                                , getPoints :: Int
                                , getDirection :: Direction
                                , isGameOver :: Bool
                                , getRandomStdGen :: StdGen
                                , getRandomStdGen2 :: StdGen
                                , getLifes:: Int
                                , getRecord::Int }

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState c w p _ g r r2 l re) newDir = GameState c w p newDir g r r2 l re

initialGameState :: Bool->Int -> GameState
initialGameState gameOver record= GameState   { getCar = [  (carX, carY), 
                                                        (carX, carY - 1),
                                                        (carX, carY - 2), 
                                                        (carX - 1, carY - 2), 
                                                        (carX + 1, carY - 2)]
                                        , getWalls =genWalls 3 (mkStdGen 100)
                                        , getPoints =0
                                        , getDirection = UP
                                        , isGameOver = gameOver
                                        , getRandomStdGen = mkStdGen 100 
                                        , getRandomStdGen2 = mkStdGen 200
                                        , getLifes=9
                                        , getRecord=record}
        where   carX = cols `div` 2
                carY = rows-21

takeThree::Int->Int->[Int]
takeThree lo hi=[res1,res2,res3]
        where
                (res1,ran1)=randomR (lo,hi) (mkStdGen 100)
                (res2,ran2)=randomR (lo,hi) ran1
                (res3,_)=randomR (lo,hi) ran2
