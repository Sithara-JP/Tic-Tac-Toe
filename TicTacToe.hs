import Data.List
import System.Environment()
data Piece
     = Open Int
     | Player Char
     deriving Eq
instance Show Piece where
     show (Open n) = show n
     show(Player c)=[c]
removeNth ::Int -> [a] -> ([a],[a]) 
removeNth index lst=(left,right) 
     where
     (left,ys)=splitAt (index-1) lst
     right =drop 1 ys

placepiece :: [a] -> a -> Int -> [a]
placepiece board piece index = xs ++ [piece] ++ys
     where 
     (xs,ys)=removeNth index board 
     
pieceisopen :: Piece -> Bool
pieceisopen (Open _)=True
pieceisopen _=False

openSpace ::[Piece] -> Int ->Bool
openSpace board index
     | length board < i         =False
     | pieceisopen $ board !! i =True
     | otherwise =False
     where i= index -1
     

getpieceposition :: [Piece] -> IO Int
getpieceposition board = do
     input <- getChar
     if input `elem` ['1' .. '9'] && openSpace board (read [input])
          then return $ (read [input])
          else do
               putStrLn $ "Enter an open position (1-9):  "
               getpieceposition board

boardline :: [Piece] -> String
boardline (a:b:c:xs) = (show a) ++ " | " ++ (show b) ++ " | " ++ (show c)
boardline _ = error "List must contain at least three elements"  

dividingLine :: String
dividingLine = "\n----------\n"
 
showboard :: [Piece] -> String
showboard board = concat $ intersperse dividingLine $ [top,middle,bottom]
     where
     top=boardline board
     middle=boardline(drop 3 board)
     bottom=boardline(drop 6 board)

swapplayer :: Char -> Char
swapplayer 'X' = 'O'
swapplayer 'O' = 'X'
swapplayer _ = error "swap player only accepts the character 'X' or 'O' "


checkverticalwin :: [Piece] -> Piece -> Int -> Bool 
checkverticalwin board player index = topPos == player && middlePos == player && bottomPos ==player
    where 
         topPos = board !! index
         middlePos= board!! (index +3)
         bottomPos=board !!(index+6)

verticalwin :: [Piece] -> Piece -> Bool
verticalwin board player = or $ map(checkverticalwin board player) [0,3,6]

checkhorizontalwin :: [Piece] -> Piece -> Int -> Bool 
checkhorizontalwin board player index = firstPos == player && secondPos == player && thirdPos ==player
    where
         firstPos = board !! index
         secondPos= board!! (index +1)
         thirdPos=board !!(index+2)
horizontalwin :: [Piece] -> Piece -> Bool
horizontalwin board player = or $ map(checkhorizontalwin board player) [0,1,2]

checkdiagonalwin :: [Piece] -> Piece -> Int -> Int  -> Bool 
checkdiagonalwin board player index step = firstPos == player && secondPos == player && thirdPos ==player
    where
         firstPos = board !! index
         secondPos= board!! (index + step)
         thirdPos=board !!(index+ 2 * step)
diagonalwin :: [Piece] -> Piece -> Bool
diagonalwin board player = wonfirstdiagonal || wonseconddiagonal
    where
         wonfirstdiagonal = checkdiagonalwin board player 0 4
         wonseconddiagonal= checkdiagonalwin board player 2 5
    
playerwon :: [Piece] -> Piece -> Bool
playerwon board player = diagonalwin board player || horizontalwin board player || verticalwin board player

tiegame :: [Piece] -> Bool
tiegame board = all(\piece -> not (pieceisopen piece)) board

checkboardstate :: [Piece] -> Char -> IO()
checkboardstate board playerChr 
     |tiegame board                = putStrLn " Draw !"
     |playerwon board (Player 'X') = putStrLn " Player X has Won The Game"
     |playerwon board (Player 'O') = putStrLn " Player O has Won The Game"
     |otherwise                    = runtictactoe board (swapplayer playerChr)

runtictactoe :: [Piece] -> Char -> IO()
runtictactoe board playerChr = do   
     putStrLn $ showboard board
     rawchoice <- getpieceposition board
     
     let newboard = placepiece board (Player playerChr) rawchoice
     checkboardstate newboard playerChr 

main :: IO ()

main = runtictactoe board 'X'
     where board=[Open 1 ,Open 2, Open 3,Open 4,Open 5 ,Open 6,Open 7,Open 8 ,Open 9]
