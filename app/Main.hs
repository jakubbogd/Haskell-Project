module Main where

import CandW
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

window :: Display
window = InWindow "Haskell Car Game" (1000, 600) (100, 100)

background :: Color
background = black

render :: GameState -> IO Picture
render gameState =return (pictures $   [ fillRectangle black (16, 0) (0,0)
                                ] ++
                                  fmap (convertToPicture colorr) car ++
                                  createwalls walls ++
                                  pointsPicture++
                                  lifesPicture++
                                  recordPicture++
                                  gameOverPicture++
                                  superPicture)
    where   car = getCar gameState 
            walls = getWalls gameState
            point = getPoints gameState
            life = getLifes gameState
            record=getRecord gameState
            super=isSuper gameState
            supercount=getSuperCount gameState
            colorr
                |super=green 
                |isGameOver gameState=red
                |otherwise=white
            convertToPicture :: Color -> (Int, Int) -> Picture
            convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
            fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                                    scale 1 (-1) $ 
                                                    translate (tx * 20 - 500) (ty * 20 - 300) $ 
                                                    rectangleSolid w h
            createwalls::Walls->[Picture]
            createwalls (w:ws)=fmap (convertToPicture blue) w++createwalls ws
            createwalls []=[]
            toFloat (x, y) = (fromIntegral x, fromIntegral y)
            gameOverPicture =   if isGameOver gameState 
                                then [  color red $ 
                                        translate (-200) 0 $ 
                                        scale 0.5 0.5 $ 
                                        text "GAME OVER"
                                     ,  color blue $ 
                                        translate (-175) (-50) $ 
                                        scale 0.2 0.2 $ 
                                        text "Press ENTER to try again." ] 
                                else []
            pointsPicture=     [color yellow $ 
                                translate 450 250 $ 
                                scale 0.25 0.25 $ 
                                text (show point)]
            recordPicture=     [color yellow $ 
                                translate 450 200 $ 
                                scale 0.25 0.25 $ 
                                text (show record)]
            lifesPicture =     [color red $ 
                                translate (-450) 250 $ 
                                scale 0.25 0.25 $ 
                                text (show life)]
            superPicture =      [color green $
                                translate (-450) 200 $ 
                                scale 0.25 0.25 $ 
                                text (show supercount)]
                                                        
update :: Float -> GameState -> IO GameState
update _ (GameState car walls points direction gameOver stdgen stdgen2 lifes record super scount) =  do
                        writeFile "record.txt" (show record)
                        if gameOver
                            then return (GameState car (genFirstWalls 3 stdgen2) points direction gameOver stdgen stdgen2 lifes record super scount)
                            else return (GameState newCar newWalls newPoints direction newGameOver newStdGen newStdGen2 newlifes newRecord super newsupercount)
    where   
            GameState newCar' newWalls' _ _ _ _ _ newlifes _ _ newsupercount'= move (GameState car walls points direction gameOver stdgen stdgen2 lifes record super scount)
            (newWalls,newStdGen,newStdGen2,wasWallsavoided) = moveWalls (GameState car newWalls' points direction gameOver stdgen stdgen2 lifes record super scount)
            newGameOver = checkGameOver (GameState car walls points direction gameOver stdgen stdgen2 lifes record super scount)
            newCar
                |wasWallsavoided=newCell newCar' newStdGen
                |otherwise =newCar'
            newPoints
                |wasWallsavoided=points+1
                |otherwise=points
            newRecord
                |record<newPoints=newPoints
                |otherwise=record
            newsupercount
                |newPoints `mod` 5==0 && newPoints-points==1=newsupercount'+5
                |otherwise =newsupercount'

handleKeys::Event->GameState->IO GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = return (changeDirection gameState LEFT)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = return (changeDirection gameState RIGHT)  
handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) gameState = return (changeDirection gameState UP)
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) gameState = return (changeDirection gameState DOWN) 
handleKeys (EventKey (Char 'z') Down _ _) (GameState a b c d e f g h i False scount) =if scount>0 then return (makeSuper (GameState a b c d e f g h i False scount) True) else return (makeSuper (GameState a b c d e f g h i False scount) False)
handleKeys (EventKey (Char 'z') Up _ _) gameState = return (makeSuper gameState False)
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState = if isGameOver gameState then return (initialGameState False record) else return gameState
    where record=getRecord gameState
handleKeys _ gameState = return gameState

main :: IO ()
main =do
    record<-fmap (read::String->Int) (readFile "record.txt") 
    playIO window background 10 (initialGameState False record) render handleKeys update
