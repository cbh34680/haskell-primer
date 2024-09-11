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



dropPairs :: [Card] -> [Card] 
dropPairs cards = f $ List.sort cards
    where
        f [] = []
        f [x] = [x]

        f (x:xs@(y:zs))
            | x == y = f zs
            | otherwise = x: f xs


test1 = do
    xss <- map List.sort <$> (splitCards 3 <$> newCardSet)
    let yss = map dropPairs xss

    putStrLn "* bedore"
    mapM_ print xss

    putStrLn "* after"
    mapM_ print yss


genPlayers :: Int -> IO [Player]
genPlayers n = do
    xss <- map dropPairs <$> (splitCards n <$> newCardSet)
    return $ zipWith Player (map show [1 .. n]) xss


main = do
    players <- genPlayers 5

    play players >>= putStrLn . ("looser is " ++) . name


play :: [Player] -> IO Player

play [] = error "too few players"
play [x] = return x

play players = do
    let
        nPlayers = length players

        -- [(takeName, giveName)] : [("1", "2"), ("2", "3"), ("3", "1")]
        matchTable = zipWith (\player opponent -> (name player, name (players !! opponent)))
            players (tail $ cycle [0 .. nPlayers - 1])

    putStrLn $ mconcat ["# match table ", show matchTable]

    nextPlayers <- winnerExits . playerMapToList <$> drawEach (playerListToMap players) matchTable

    {-
    print players
    print . sum $ map (length . hands) players

    print nextPlayers
    print . sum $ map (length . hands) nextPlayers
    -}

    play nextPlayers


playerListToMap :: [Player] -> Map.Map String Player
playerListToMap = Map.fromList . map (\player -> (name player, player))


playerMapToList :: Map.Map String Player -> [Player]
playerMapToList = map snd . Map.toList


winnerExits :: [Player] -> [Player]
winnerExits = filter (not . null . hands)


drawEach :: Map.Map String Player -> [(String, String)] ->  IO (Map.Map String Player)

drawEach playerMap [] = return playerMap
drawEach playerMap ((takeName, giveName):nextMatchTable) = do
    let
        takePlayer = fromJust $ Map.lookup takeName playerMap
        givePlayer = fromJust $ Map.lookup giveName playerMap

    if null (hands takePlayer) || null (hands givePlayer)
        then drawEach playerMap nextMatchTable

        else do
            giveHands <- shuffleM $ hands givePlayer
            sel <- randomRIO (0, length giveHands - 1)

            let
                (selCard, restHands) = dropOut sel giveHands

                nextTakePlayer = takePlayer {hands=dropPairs (selCard: hands takePlayer)}
                nextGivePlayer = givePlayer {hands=restHands}

                nextPlayerMap = foldr (\x r -> Map.insert (name x) x r) playerMap
                                    [nextGivePlayer, nextTakePlayer]

            putStrLn "==>>>"
            putStrLn $ mconcat ["# take '", takeName, "' <-- Card -- give '", giveName, "'"]
            putStrLn ""
            putStrLn "* before"
            print takePlayer
            print givePlayer
            putStrLn ""
            putStrLn $ mconcat ["# shuffled", show giveHands]
            putStrLn $ mconcat ["# select[", show sel, "] card is ", show selCard]
            putStrLn ""
            putStrLn "* after"
            print nextTakePlayer
            print nextGivePlayer
            putStrLn "<<<=="

            drawEach nextPlayerMap nextMatchTable


dropOut :: Int -> [Card] -> (Card, [Card])
dropOut n cards =
    let
        ls = take n cards
        rs = drop (n + 1) cards
        card = cards !! n
    in
        (card, ls ++ rs)


-- EOF
