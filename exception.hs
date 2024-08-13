-- これはパースエラーを表現する型です
data ParseError = Err {location::Int, reason::String}

-- これをエラークラスのインスタンスとします
instance Error ParseError where
  noMsg    = Err 0 "Parse Error"
  strMsg s = Err 0 s

-- モナド型構築子については、Either ParseError を使います
-- これは Left ParserError で失敗を表現し、
-- 型 a の成功した結果は Right a で表現します
type ParseMonad = Either ParseError

-- parseHexDigit は ParseMonad モナド内で、単一の16進数を 
-- Integer に変換し、個々の不正文字に対してエラーを投げます
parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c idx = if isHexDigit c then
                        return (toInteger (digitToInt c))
                      else
                        throwError (Err idx ("Invalid character '" ++ [c] ++ "'"))

-- parseHex 16進数の文字列をパースし ParseMonad モナド内で
-- Integer に変換します。parseHexDigit からのパースエラーは
-- parseHex からの例外復帰を引き起こします。
parseHex :: String -> ParseMonad Integer
parseHex s = parseHex' s 0 1
  where parseHex' []      val _   = return val
        parseHex' (c:cs)  val idx = do d <- parseHexDigit c idx
                                       parseHex' cs ((val * 16) + d) (idx + 1)

-- toString は Integer を ParseMonad モナド内で String に変換します
toString :: Integer -> ParseMonad String
toString n = return $ show n

-- convert は16進数文字列を10進文字列に変換します。
-- 入力文字列上でのパースエラーは出力文字列として
-- エラーの説明メッセージを生成します。
convert :: String -> String
convert s = let (Right str) = do {n <- parseHex s; toString n} `catchError` printError
            in str
  where printError e = return $ "At index " ++ (show (location e)) ++ ":" ++ (reason e)

