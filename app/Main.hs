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
                                  fmap (convertToPicture white) car ++ 
                                  fmap (convertToPicture blue) wall ++
                                  pointsPicture++
                                  lifesPicture++
                                  recordPicture++
                                  gameOverPicture)
    where   car = getCar gameState 
            wall = getWall gameState
            point = getPoints gameState
            life = getLifes gameState
            record=getRecord gameState
            convertToPicture :: Color -> (Int, Int) -> Picture
            convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
            fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                                    scale 1 (-1) $ 
                                                    translate (tx * 20 - 500) (ty * 20 - 300) $ 
                                                    rectangleSolid w h
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
                                                        
update :: Float -> GameState -> IO GameState
update _ gameState =  do
                        writeFile "record.txt" (show record)
                        if gameOver
                            then return gameState
                            else return (GameState newCar newWall newPoints direction newGameOver newStdGen newlifes newRecord)
    where   points =getPoints gameState
            direction = getDirection gameState
            gameOver = isGameOver gameState
            record = getRecord gameState
            GameState newCar _ _ _ _ _ newlifes _= move gameState
            (newWall,newStdGen,wasWallavoided) = moveWall gameState
            newGameOver = checkGameOver gameState
            newPoints
                |wasWallavoided=points+1
                |otherwise=points
            newRecord
                |record<newPoints=newPoints
                |otherwise=record
            save=newRecord>=record  

handleKeys::Event->GameState->IO GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = return (changeDirection gameState LEFT)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = return (changeDirection gameState RIGHT)  
handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) gameState = return (changeDirection gameState UP)
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) gameState = return (changeDirection gameState DOWN) 
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState =    if isGameOver gameState then return (initialGameState False record) else return gameState
    where record=getRecord gameState
handleKeys _ gameState = return gameState

main :: IO ()
main =do
    a<-fmap (read::String->Int) (readFile "record.txt") 
    playIO window background 10 (initialGameState False a) render handleKeys update
