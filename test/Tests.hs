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
            carY<-chooseInt (5,rows-18)
            width<-chooseInt (2,5)
            x<-chooseInt (1,cols `div` 2)
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

    fun2::GameState ->Bool
    fun2 (GameState [] w _ _ _ _ _ _ _ _ _)=not (wasWallHit [] (head w))
    fun2 (GameState c [] _ _ _ _ _ _ _ _ _)=not (wasWallHit c [])
    fun2 (GameState car walls _ _ _ _ _ _ _ _ _)=wasWallHit car wall==wasWallhit
        where
                wasWallhit =(minx<=wallX1 && maxx>=wallX1 ||minx<=wallX2 && maxx>=wallX2 ||minx>=wallX1 && maxx<=wallX2) && wallY==headY-2||
                            headX<=wallX2 && headX>=wallX1 &&wallY==headY ||
                            headX<=wallX2 && headX>=wallX1 &&wallY==(headY-1)
                (headX, headY) = head car
                (wallX1,wallY) = head wall
                (wallX2,_) = last wall
                minx=minimum (getHeads' car)
                maxx=maximum (getHeads' car)
                wall=head walls

    fun3::GameState ->Bool
    fun3 (GameState car [] a b c d e f g h i)=not (wasWallsHit (GameState car [] a b c d e f g h i))
    fun3 (GameState car (w:ws) a b c d e f g h i)=wasWallsHit (GameState car (w:ws) a b c d e f g h i)==wasWallHit car w||fun3 (GameState car ws a b c d e f g h i)

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
    fun5 (GameState car wall a b c d e f g h i)= if not (fun3 (GameState car wall a b c d e f g h i))
                                                then car==gc && f==(gl+1)
                                                else f==gl && gc==move' (directionVectorMap ! b) car
                                            where
                                                gameState=move (GameState car wall a b c d e f g h i)
                                                gc=getCar gameState
                                                gl=getLifes gameState


    fun6::Int->Int->Int->Int->Bool
    fun6 x y rn w=temp2 (genWall x y (abs rn) (abs w) ) (abs w)
    temp2::Wall->Int->Bool
    temp2 ws w=length ws ==(w+1)

    fun7::GameState->Direction->Bool
    fun7 (GameState a b c d e f g h i j k) dir=getDirection (changeDirection (GameState a b c d e f g h i j k) dir)==dir

    fun8::Car->Bool
    fun8 []=Prelude.null (getHeads' [])
    fun8 car=length (getHeads' car)==length car

    fun9::GameState ->Bool
    fun9 (GameState car walls _ _ _ _ _ _ _ _ _)=not (temp3 res car)
        where res=deletewalls car walls

    temp3::Walls->Car->Bool
    temp3 (r:res) car= temp4 r car||temp3 res car
    temp3 [] _=False

    temp4::Wall->Car->Bool
    temp4 (w:ws) car=w `elem` car||temp4 ws car
    temp4 [] _=False

    fun10::GameState->Bool
    fun10 (GameState car _ _ _ _ ran _ _ _ _ _)=length (newCell car ran)==length car+1 && abs (minx1-minx2)<=1 &&  abs (maxx1-maxx2)<=1
        where minx1=minimum (getHeads' car)
              maxx1=maximum (getHeads' car)
              minx2=minimum (getHeads' (newCell car ran))
              maxx2=maximum (getHeads' (newCell car ran))

    fun11::[Int]-> [Int]-> [Int]-> [Int]->Bool
    fun11 [] b c d =Prelude.null (genWalls [] b c d)
    fun11 a [] c d =Prelude.null (genWalls a [] c d)
    fun11 a b [] d =Prelude.null (genWalls a b [] d)
    fun11 a b c [] =Prelude.null (genWalls a b c [])
    fun11 a b c d =not (length a==length b && length b ==length c && length c==length d) || (length (genWalls a b c d)==length a)

    test::IO()
    test=quickCheck fun1>>quickCheck fun2>>quickCheck fun3>>quickCheck fun4>>quickCheck fun5>>quickCheck fun6>>quickCheck fun7>>quickCheck fun8>>quickCheck fun9>>quickCheck fun10>>quickCheck fun11
