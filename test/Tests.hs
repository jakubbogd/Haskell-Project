{-# OPTIONS_GHC -Wno-orphans #-}
module Tests where

    import Test.QuickCheck
    import System.Random
    import Data.Map
    import CandW

    instance Arbitrary Direction where
        arbitrary=elements [UP,DOWN,LEFT,RIGHT]

    instance Arbitrary GameState where
        arbitrary=do
            carX<-chooseInt (2,cols-2)
            carY<-chooseInt (3,rows-19)
            width<-chooseInt (2,5)
            x<-choose (1,cols `div` 2)
            record<-chooseInt (1,6)
            point<-chooseInt (1,record)
            dir<-elements [UP,DOWN,LEFT,RIGHT]
            go<-elements [True,False]
            r<-chooseInt (0,100)
            r'<-chooseInt (0,200)
            l<-chooseInt (1,9)
            super<-elements [True, False]
            supercount<-chooseInt (1,5)
            return (GameState (car carX carY) (walls x width) point dir go (mkStdGen r) (mkStdGen r') l record super supercount)
            where car carX carY=[(carX, carY),(carX, carY - 1),(carX, carY - 2),(carX - 1, carY - 2),(carX + 1, carY - 2)]
                  walls x width=[[((min (max (x+i) 0) (min (x+i) cols)+max (max (x+i) 0) (min (x+i) cols)) `div` 2,3-j)|i<-[0..width]]|j<-[0,5,10,15,20]]
                               



    fun1::Direction->Bool
    fun1 UP=directionVectorMap ! UP==(0,-1)
    fun1 DOWN=directionVectorMap ! DOWN==(0,1)
    fun1 LEFT=directionVectorMap ! LEFT==(-1,0)
    fun1 RIGHT=directionVectorMap ! RIGHT==(1,0)

    fun2::Car->Wall->Bool
    fun2 [] w=not (wasWallHit [] w)
    fun2 c []=not (wasWallHit c [])
    fun2 car wall=wasWallHit car wall==wasWallhit
        where
                wasWallhit = headX<=wallX2 && headX>=wallX1 &&wallY==headY ||
                        headX<=wallX2 && headX>=wallX1 &&wallY==(headY-1)||
                        wallY==headY-2 && (wallX1<=headX-1 && headX-1<=wallX2 || wallX1<=headX && headX<=wallX2 || wallX1<=headX+1 && headX+1<=wallX2)
                (headX, headY) = head car
                (wallX1,wallY) = head wall
                (wallX2,_) = last wall

    fun3::GameState ->Bool
    fun3 (GameState car [] a b c d e f g h i j)=not (wasWallsHit (GameState car [] a b c d e f g h i j))
    fun3 (GameState car (w:ws) a b c d e f g h i j)=wasWallsHit (GameState car (w:ws) a b c d e f g h i j)==(fun2 car w||fun3 (GameState car ws a b c d e f g h i j))

    fun4::(Int,Int)->Car->Bool
    fun4 (a,b) []=Prelude.null (move' (a,b) [])
    fun4 (a,b) xs=temp (move' (a,b) xs) (a,b) xs

    temp::Car->(Int,Int)->Car->Bool
    temp ((c1,c2):cs) (a,b) ((x1,x2):xs)= (c1==a+x1 && c2==b+x2)||temp cs (a,b) xs
    temp [(_, _)] (_, _) []=False
    temp ((_, _):_:_) (_, _) []=False
    temp [] (_, _) []=False
    temp [] (_, _) (_:_)=False

    fun5::GameState ->Bool 
    fun5 (GameState car wall a b c d e f g h i j)= if fun3 (GameState car wall a b c d e f g h i j)
                                                then car==gc && f==(gl+1)
                                                else f==gl && gc==move' (directionVectorMap ! b) car
                                            where
                                                gameState=move (GameState car wall a b c d e f g h i j) 
                                                gc=getCar gameState
                                                gl=getLifes gameState


    fun6::Int->Int->Int->Int->Bool 
    fun6 x y rn 0=Prelude.null (gennewWall x y rn 0)
    fun6 x y rn w=temp2 (gennewWall x y (abs rn) (abs w) ) (abs w)
    temp2::Wall->Int->Bool
    temp2 ws w=length ws ==(w+1)
    fun7::GameState->Direction->Bool 
    fun7 (GameState a b c d e f g h i j k) dir=getDirection (changeDirection (GameState a b c d e f g h i j k) dir)==dir

    fun8::Wall->Bool 
    fun8 []=getHead []==0
    fun8 wall=getHead wall==temp3 (head wall)

    temp3::(Int,Int)->Int
    temp3 (a,_)=a

    test::IO()
    test=quickCheck fun1>>quickCheck fun2>>quickCheck fun3>>quickCheck fun4>>quickCheck fun5>>quickCheck fun6>>quickCheck fun7>>quickCheck fun8