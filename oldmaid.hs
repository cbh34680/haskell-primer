import System.Random.Shuffle
import System.Random
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe



data Suit = Hearts | Diamonds | Clubs | Spades deriving (Enum, Eq, Ord)
data Card = NCard { suit::Suit, code::Int } | Joker
data Player = Player { name::String, hands::[Card] } deriving Show


instance Show Suit where
    show Hearts = "H"
    show Diamonds = "D"
    show Clubs = "C"
    show Spades = "S"

instance Show Card where
    show (NCard s n) = mconcat [ show s, "(", show n, ")" ]
    show Joker = "Joner"


instance Eq Card where
    (NCard _ a) == (NCard _ b) = a == b
    _ == _ = False


instance Ord Card where
    compare (NCard _ a) (NCard _ b) = compare a b
    compare Joker Joker = EQ

    compare (NCard _ _) _ = LT
    compare _ _ = GT


newCardSet :: IO [Card]
newCardSet = shuffleM allCards
    where
        allCards = [ NCard suit' code'
                        | suit' <- [Hearts ..], code' <- [1..13] ] ++ [Joker]


splitCards :: Int -> [a] -> [[a]]
splitCards n xs = List.transpose $ f xs
    where
        f [] = []
        f ys = take n ys : f (drop n ys)



dropPairs [] = []
dropPairs [x] = [x]

dropPairs (x:xs@(y:zs))
    | x == y = dropPairs zs
    | otherwise = x: dropPairs xs


test1 = do
    xss <- map List.sort <$> (splitCards 3 <$> newCardSet)
    let yss = map dropPairs xss

    putStrLn "* bedore"
    mapM_ print xss

    putStrLn "* after"
    mapM_ print yss


genPlayers :: Int -> IO [Player]
genPlayers n = do
    xss <- map (dropPairs . List.sort) <$> (splitCards n <$> newCardSet)
    return $ zipWith Player (map show [1 .. n]) xss



main = do
    players <- genPlayers 3

    play players >>= putStrLn . ("looser is " ++) . name


play :: [Player] -> IO Player

play [] = error "too few players"
play [x] = return x

play players = do
    let
        nPlayers = length players
        fromTo = zipWith (\player opponent -> (name player, name (players !! opponent)))
            players (tail $ cycle [0 .. nPlayers - 1])
        playerMap = Map.fromList $ map (\player -> (name player, player)) players

    print fromTo
    print playerMap

    newPlayerMap <- draw playerMap fromTo

    let
        newPlayers = filter (not . null . hands) . map snd $ Map.toList newPlayerMap

    print players
    print . sum $ map (length . hands) players

    print newPlayers
    print . sum $ map (length . hands) newPlayers

    play newPlayers



draw :: Map.Map String Player -> [(String, String)] ->  IO (Map.Map String Player)

draw playerMap [] = return playerMap
draw playerMap ((from, to):xs) = do
    let
        takePlayer = fromJust $ Map.lookup from playerMap
        givePlayer = fromJust $ Map.lookup to playerMap

    if null (hands takePlayer) || null (hands givePlayer) then draw playerMap xs else
        do
            giveHands <- shuffleM $ hands givePlayer
            giveTarget <- randomRIO (0, length giveHands - 1)

            let
                ls = take giveTarget giveHands
                rs = drop (giveTarget + 1) giveHands
                giveCard = giveHands !! giveTarget

                newFromPlayer = takePlayer {hands=dropPairs . List.sort $ giveCard:hands takePlayer}
                newToPlayer = givePlayer {hands=List.sort $ ls ++ rs}
                newPlayerMap = Map.insert to newToPlayer $ Map.insert from newFromPlayer playerMap

            putStrLn "==>>>"
            print takePlayer
            print givePlayer
            print giveHands
            print giveTarget
            print ls
            print giveCard
            print rs
            print newFromPlayer
            print newToPlayer
            print newPlayerMap
            putStrLn "<<<=="

            draw newPlayerMap xs








-- EOF
