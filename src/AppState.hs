module AppState where
    import System.Random
    import Control.Monad.Trans.Class
    import Control.Monad.Trans.State.Strict

    data Direction = UP | DOWN |LEFT | RIGHT deriving (Eq, Ord)
    type Wall = [(Int, Int)]
    type Car = [(Int, Int)]

    data GameState = GameState   { getCar :: Car
                                , getWall :: Wall
                                , getPoints :: Int
                                , getDirection :: Direction
                                , getRandomStdGen :: StdGen }

    type GameIO a = StateT GameState IO a

    parseCommand :: Char -> Maybe Direction
    parseCommand 'a' = Just LEFT
    parseCommand 's' = Just DOWN
    parseCommand 'w' = Just UP
    parseCommand 'd' = Just RIGHT
    parseCommand _ = Nothing

    gameStart::GameIO ()
    gameStart =
        return ()


    readCommand::GameIO (Maybe Direction)
    readCommand= do
        x<- lift getChar
        let r = parseCommand x
        return r

    makeMove::Maybe Direction->Car-> Car
    makeMove Nothing car=car
    makeMove _ []=[]
    makeMove (Just LEFT) ((a,b):xs)=(a-1,b):makeMove (Just LEFT) xs
    makeMove (Just RIGHT) ((a,b):xs)=(a+1,b):makeMove (Just RIGHT) xs
    makeMove (Just UP) ((a,b):xs)=(a,b+1):makeMove (Just UP) xs
    makeMove (Just DOWN) ((a,b):xs)=(a,b-1):makeMove (Just DOWN) xs


    runCommand :: Maybe Direction -> GameIO ()
    runCommand (Just dir) = modify' (\(GameState a b c d e) -> GameState (makeMove (Just dir) a) b c d e)
    runCommand Nothing = modify' id

    accident ::Car->Wall->Bool
    accident ((a,b):xs) wall= accidentcell (a,b) wall || accident xs wall
    accident [] _ =False


    accidentcell::(Int, Int)->[(Int, Int)]->Bool
    accidentcell (a,b) ((c,d):ys)=(a==c && b==d) || accidentcell (a,b) ys
    accidentcell (_,_) [] =False
   

    checkEnd::GameIO Bool
    checkEnd = do
        GameState a b _ _ _ <- get
        let res=accident a b
        if res then return True else return False

    mainLoop::GameIO ()
    mainLoop = readCommand >>= runCommand >> checkEnd
        >>=( \p ->
        if p then (lift $ putStr "Game Over\n")>>return () else  mainLoop )
    

    toMain :: IO ()
    toMain =  do
        _ <- execStateT mainLoop firstState
        return ()

    firstState::GameState
    firstState=GameState   { getCar = [  (carX, carY), 
                                                        (carX, carY - 1),
                                                        (carX, carY - 2), 
                                                        (carX - 1, carY - 2), 
                                                        (carX + 1, carY - 2)]
                                        , getWall = [(carX-2,rows-1),(carX-1,rows-1),(carX,rows-1),(carX+1,rows-1), (carX+2,rows-1)]
                                        , getPoints =0
                                        , getDirection = LEFT
                                        , getRandomStdGen = mkStdGen 100 }
                where   carX = cols `div` 2
                        carY = 4
    
    cols::Int
    rows::Int
    cols = 50
    rows = 33