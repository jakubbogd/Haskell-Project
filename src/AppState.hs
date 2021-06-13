module CandW where

import Data.Map as Map
import System.Random

data Direction = UP | DOWN |LEFT | RIGHT deriving (Eq, Ord,Show)
type Wall = [(Int, Int)]
type Walls=[Wall]
type Car = [(Int, Int)]
type Shot=(Int,Int)

cols::Int
rows::Int
cols = 50
rows = 50

data GameState = GameState  { getCar :: Car
                        , getWalls :: Walls
                        , getPoints :: Int
                        , getDirection :: Direction
                        , isGameOver :: Bool
                        , getRandomStdGen :: StdGen
                        , getRandomStdGen2 :: StdGen
                        , getLifes:: Int
                        , getRecord::Int
                        , isSuper:: Bool
                        , getSuperCount::Int} deriving Show

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList $ zip [UP,DOWN, LEFT, RIGHT] [(0, -1), (0, 1),(-1, 0), (1, 0)]

wasWallsHit::GameState->Bool
wasWallsHit (GameState car (w:ws) a b c d e f g super i)= wasWallHit car w || wasWallsHit (GameState car ws a b c d e f g super i)
wasWallsHit (GameState _ [] _ _ _ _ _ _ _ _ _)=False

wasWallHit::Car->Wall->Bool
wasWallHit [] _=False
wasWallHit _ []=False
wasWallHit [c] w=c `elem` w 
wasWallHit (c:cs) w=c `elem` w || wasWallHit cs w

deletewalls::Car->Walls->Walls
deletewalls car (w:ws)=deletewall car w:deletewalls car ws
deletewalls _ [] =[]

deletewall::Car->Wall->Wall
deletewall car= Prelude.filter (`notElem` car)

move :: GameState->GameState
move (GameState car wall a dir aa aaa aaaaa lifes aaaa super supercount)= if wasWallsHit (GameState car wall a dir aa aaa aaaaa lifes aaaa super supercount)
                            then if super
                                    then if (supercount-1)<1
                                        then GameState newcar newwall a dir aa aaa aaaaa (lifes-1) aaaa False (max (supercount-1) 0)
                                        else GameState newcar newwall a dir aa aaa aaaaa lifes aaaa True (max (supercount-1) 0)
                                    else GameState newcar newwall a dir aa aaa aaaaa (lifes-1) aaaa super supercount
                            else GameState newcar wall a dir aa aaa aaaaa lifes aaaa super supercount
    where   newcar = move' (directionVectorMap ! dir) car
            newwall=deletewalls car wall

gennewWall::Int->Int->Int->Int->Wall
gennewWall _ _ _ 0=[]
gennewWall x y rn w=[((min (max (x+rn+i) 0) (min (x+rn+i) cols)+max (max (x+rn+i) 0) (min (x+rn+i) cols)) `div` 2,y)|i<-[0..w]]

gennewWalls::[Int]->[Int]->[Int]->[Int]->Walls
gennewWalls (x:xs) (y:ys) (r:rs) (w:ws)=gennewWall x y r w:gennewWalls xs ys rs ws
gennewWalls [] _ _ _ =[]
gennewWalls _ [] _ _ =[]
gennewWalls _ _ [] _ =[]
gennewWalls _ _ _ [] =[]

genWalls::Int->StdGen->Walls
genWalls y ran=[gennewWall ((cols `div` width)+(i*2*width)) (y-j) 0  width|i<-takeThree 1 2,j<-[0,5,10,15,20]]
        where (width,_)=randomR (3,7) ran

moveWalls:: GameState-> (Walls,StdGen,StdGen,Bool)
moveWalls (GameState _ walls _ _ _ ran _ _ _ _ _)= (newWalls,ran',ran'',newPoint)
        where
                (_,wallY)=head (head walls)
                wallXs=getHeads walls
                (wallY',rm)
                        |wallY==rows = (1,7)
                        |otherwise =(wallY,1)
                wallYs=getWallYs walls (wallY-wallY')
                newWalls
                        |wallY'==1 = genWalls 3 ran''
                        |otherwise =gennewWalls wallXs wallYs rns widths
                newPoint
                        |rm==1=False
                        |otherwise =True
                (_,ran')=randomR (-1*rm, 1*rm) ran
                (_,ran'')=randomR (-1*rm, 1*rm) ran'
                rns=getRandoms ran walls rm
                widths=getWidths ran walls wallY

checkGameOver :: GameState -> Bool
checkGameOver (GameState car wall point dir over ran ran2 lifes record super supercount) =   headX == 2 || headX == (cols-2) ||
                      headY == (rows-19)||headY==3||
                      (wasWallsHit (GameState car wall point dir over ran ran2 lifes record super supercount) && lifes<2)
    where       (headX, headY) = head car

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState c w p _ g r r2 l re su suc) newDir = GameState c w p newDir g r r2 l re su suc

makeSuper::GameState->Bool->GameState
makeSuper (GameState a b c d e f g i j _ k) bool=GameState a b c d e f g i j bool k

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
                                        , getRecord=record
                                        , isSuper=False
                                        , getSuperCount=5}
        where   carX = cols `div` 2
                carY = rows-21

takeThree::Int->Int->[Int]
takeThree lo hi=[res1,res2,res3]
        where
                (res1,ran1)=randomR (lo,hi) (mkStdGen 100)
                (res2,ran2)=randomR (lo,hi) ran1
                (res3,_)=randomR (lo,hi) ran2

getn::Int->[a]->a
getn n xs = last (Prelude.take n xs)

getWallYs::Walls->Int->[Int]
getWallYs (w:ws) diff=(y-diff+1):getWallYs ws diff
        where (_,y)=head w
getWallYs [] _=[]

getRandoms::StdGen->Walls->Int->[Int]
getRandoms ran (_:ws) rm=rn:getRandoms ran2 ws rm
        where (rn,ran2)=randomR (-1*rm, 1*rm) ran
getRandoms _ [] _=[]

getWidths::StdGen->Walls->Int->[Int]
getWidths ran (w:ws) y=width:getWidths ran2 ws y
        where (width,ran2)
                        |y<2=randomR (1,cols `div` 2) ran
                        |otherwise=(length w-1, ran)
getWidths _ [] _=[]

getHeads::Walls->[Int]
getHeads = Prelude.map getHead

getHead::Wall->Int
getHead ((a,_):_) =a
getHead [] =0

getHeads'::Car->[Int]
getHeads' ((a,_):cs)=a:getHeads' cs
getHeads' []=[]

move':: (Int,Int)->Car->Car
move' (_,_) []=[]
move' (a,b) ((c,d):xs) =(a+c,b+d): move' (a,b) xs

rantemp::StdGen->Int
rantemp ran=res
        where (res,_)=randomR (0,4) ran
newCell::Car->StdGen->Car
newCell car ran=newcar
        where temp=rantemp ran
              (_,carY)=head car
              newcar
                |temp<3=car++[(minimum (getHeads' car)-1,carY-2)]
                |otherwise=car++[(maximum (getHeads' car)+1,carY-2)]
