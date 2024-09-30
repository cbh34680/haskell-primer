import Text.Parsec

readInt :: String -> Int
readInt = read


parser :: Parsec String () [Int]
parser = spaces *> many parseNum <* eof


parseNum :: Parsec String () Int
parseNum =
    -- ( 2 3 ) ==> 5
    try parseSumBracket
    <|>
    -- { 2 3 } ==> 6
    parseProductBracket
    <|>
    -- 1 ==> 1
    readInt <$> (many1 digit <* spaces)


parseSumBracket :: Parsec String () Int
parseSumBracket =
    char '(' *> spaces *> (sum <$> many parseNum) <* char ')' <* spaces


parseProductBracket :: Parsec String () Int
parseProductBracket =
    char '{' *> spaces *> (product <$> many parseNum) <* char '}' <* spaces


main = do
    print $ parse parser "(src)" "  1 2 3 4 5  "
    print $ parse parser "(src)" "  1 2 ( 3 4 ) 5  "
    print $ parse parser "(src)" "  1 2 { 3 4 } 5  "
    print $ parse parser "(src)" "  1 a 3 "


-- EOF
