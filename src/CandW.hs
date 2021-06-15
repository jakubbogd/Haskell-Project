module CandW where

import Data.Map as Map
import System.Random

data Direction = UP | DOWN |LEFT | RIGHT deriving (Eq, Ord,Show)
type Wall = [(Int, Int)]
type Walls=[Wall]
type Car = [(Int, Int)]

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
wasWallsHit (GameState car walls _ _ _ _ _ _ _ _ _)= Prelude.foldr (\w x->x|| wasWallHit car w) False walls

wasWallHit::Car->Wall->Bool
wasWallHit car w=Prelude.foldr (\c x->x||c `elem` w) False car

deletewalls::Car->Walls->Walls
deletewalls car =Prelude.map (Prelude.filter (`notElem` car))

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

genWall::Int->Int->Int->Int->Wall
genWall x y rn w=[((min (max (x+rn+i) 0) (min (x+rn+i) cols)+max (max (x+rn+i) 0) (min (x+rn+i) cols)) `div` 2,y)|i<-[0..w]]

genWalls::[Int]->[Int]->[Int]->[Int]->Walls
genWalls (x:xs) (y:ys) (r:rs) (w:ws)=genWall x y r w:genWalls xs ys rs ws
genWalls [] _ _ _ =[]
genWalls _ [] _ _ =[]
genWalls _ _ [] _ =[]
genWalls _ _ _ [] =[]

genFirstWalls::Int->StdGen->Walls
genFirstWalls y ran =rmdups (genWalls (replist 5 [(cols `div` width)+(i*2*width)| i<-taken n 1 2 ran4]) (concat [replicate n (y-j)|j<-genList m (-5)]) (replicate 15 (-5)) (replicate 15 width))
         where (width,ran2)=randomR (4,7) ran
               (n,ran3)=randomR (3,5) ran2
               (m,ran4)=randomR (3,5) ran3

genList::Int->Int->[Int]
genList n x=if n==1 then [x+5] else (x+5):genList (n-1) (x+5) 

moveWalls:: GameState-> (Walls,StdGen,StdGen,Bool)
moveWalls (GameState _ [] _ _ _ ran ran' _ _ _ _)=([],ran,ran',False)
moveWalls (GameState _ walls _ _ _ ran _ _ _ _ _)= (newWalls,ran',ran'',newPoint)
        where
                (_,wallY)=head (head walls)
                wallXs=getHeads walls
                (wallY',rm,newPoint)
                        |wallY==rows = (1,7,True)
                        |otherwise =(wallY,1,False)
                wallYs=getWallYs walls (wallY-wallY')
                newWalls
                        |wallY'==1 = genFirstWalls 2 ran''
                        |otherwise =genWalls wallXs wallYs rns widths
                (_,ran')=randomR (-1*rm, 1*rm) ran
                (_,ran'')=randomR (-1*rm, 1*rm) ran'
                rns=getRandoms ran walls rm
                widths=getWidths ran walls wallY

checkGameOver :: GameState -> Bool
checkGameOver (GameState car wall point dir over ran ran2 lifes record super supercount) =   minx == 1 || maxx == cols-1 ||
                      headY == (rows-19)||headY==3||
                      (wasWallsHit (GameState car wall point dir over ran ran2 lifes record super supercount) && lifes<1)||lifes==0
        where   (_, headY) = head car
                minx=minimum (getHeads' car)
                maxx=maximum (getHeads' car)

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState c w p _ g r r2 l re su suc) newDir = GameState c w p newDir g r r2 l re su suc

makeSuper::GameState->Bool->GameState
makeSuper (GameState a b c d e f g i j _ k) bool=GameState a b c d e f g i j bool k

newCell::Car->StdGen->Car
newCell car ran=newcar
        where
              (_,carY)=head car
              minx=minimum (getHeads' car)-1
              maxx=maximum (getHeads' car)+1
              newcar=if maxx-minx<cols `div` 2 then (if getRandom ran<3 then car++[(minx,carY-2)] else car++[(maxx,carY-2)]) else car

initialGameState :: Bool->Int -> GameState
initialGameState gameOver record=GameState { getCar = [ (carX, carY),
                                                        (carX, carY - 1),
                                                        (carX, carY - 2),
                                                        (carX - 1, carY - 2),
                                                        (carX + 1, carY - 2)]
                                        , getWalls =genFirstWalls 2 (mkStdGen 10)
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

taken::Int->Int->Int->StdGen->[Int]
taken n lo hi ran=if n==1 then [res] else res:taken (n-1) lo hi ran2
        where (res,ran2)=randomR (lo,hi) ran

getWallYs::Walls->Int->[Int]
getWallYs walls diff =Prelude.map (\((_,y):_)->y-diff+1) walls

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
getHeads= Prelude.map (\((a,_):_)->a)

getHeads'::Car->[Int]
getHeads'= Prelude.map fst

move':: (Int,Int)->Car->Car
move' (a,b)= Prelude.map (\(c,d)->(a+c,b+d))

getRandom::StdGen->Int
getRandom ran=res
        where (res,_)=randomR (0,4) ran

replist:: Int-> [Int]->[Int]
replist n x= if n==1 then x else x ++ replist (n-1) x

rmdups :: Eq a=>[a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : Prelude.filter (/= x) (rmdups xs)
