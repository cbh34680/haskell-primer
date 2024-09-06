import Control.Monad
import System.Random
import System.Random.Shuffle
import Data.Bool
import Data.Maybe
import Data.List
import qualified Data.Ord as Ord

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Enum)
data NumOfSuit = A | N Int | J | Q | K deriving Show
data Card = Card { suit::Suit, numOfSuit::NumOfSuit } deriving Show

data Player = Player {
                name::String, stop::Bool,
                times::Int, threshold::Int,
                hands::[Card] } deriving Show


toNumbers :: NumOfSuit -> [Int]
toNumbers A = [1, 11]
toNumbers (N x) = [x]
toNumbers _ = [10]


maxScore :: [Card] -> Maybe Int
maxScore xs
    | null xs = Just 0
    | otherwise =
        let
            --ys = filter (<= 21) . map sum . sequence $ map (toNumbers . numOfSuit) xs
            ys = filter (<= 21) . map sum $ mapM (toNumbers . numOfSuit) xs
        in
            case ys of
                [] -> Nothing
                _  -> Just (maximum ys)


genCards :: IO [Card]
genCards = shuffleM [ Card suit' numOfSuit' | suit' <- [Hearts ..], numOfSuit' <- genNumbers ]
    where
        genNumbers = A : map N [2..10] ++ [J, Q, K]


main = do
    {-
    let x = Player "abc" False 4 4 [Card Hearts (N 3), Card Clubs A, Card Spades A]
    print x
    print $ maxScore (hands x)
    -}

    initPlayers <- mapM newPlayer [0..3]
    initCards <- genCards

    putStrLn "=== init ===>>>"
    mapM_ print initCards
    print initPlayers
    putStrLn "=== init ===<<<"

    lastPlayers <- play initCards initPlayers

    putStrLn "=== results ===>>>"
    mapM_ (\x -> print x >> putStrLn "") lastPlayers
    putStrLn "=== results ===<<<"

    print $ winner lastPlayers


winner players =
    let
        --xs = groupBy (\a b -> fst a == fst b) . reverse . sort . catMaybes $
        xs = groupBy (\a b -> fst a == fst b) . sortBy (Ord.comparing Ord.Down) . catMaybes $
            --zipWith (\a b -> fmap (\n -> (n, b)) a)
            zipWith (\a b -> fmap (, b) a)
                (map (maxScore . hands) players) (map name players)
    in
        case xs of
            [] -> Nothing
            (y:_) -> Just y


newPlayer :: Int -> IO Player
newPlayer n = do
    times' <- randomRIO (1, 5)
    threshold' <- randomRIO (15, 18)

    return $ Player {
                name=show (n + 1), stop=False,
                times=times', threshold=threshold',
                hands=[] }


play :: [Card] -> [Player] -> IO [Player]
play cards players = do
    -- let fin = (== 0) . length $ filter (not . stop) players
    -- let fin = null $ filter (not . stop) players
    let fin = any (not . stop) players

    if fin then return players else
        do
            let lenCards = sum . map (length . hands)

            let nextPlayers = decide cards players
            let consumed = lenCards nextPlayers - lenCards players

            play (drop consumed cards) nextPlayers


decide :: [Card] -> [Player] -> [Player]

decide [] _ = error "no card"
decide (card:cards) [] = []

decide allcards@(card:cards) (player:players)
    | True <- stop player = player : decide allcards players
    | otherwise =
        case maxScore (hands player) of
            Nothing -> player { stop=True } : decide allcards players
            Just score ->
                if score < threshold player then
                    player { hands=card:hands player } : decide cards players
                    else
                        player { stop=True } : decide allcards players


-- EOF
