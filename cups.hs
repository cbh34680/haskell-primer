
newCup num = (\getter -> getter num)

getNum obj = obj (\num -> num)

drink obj num = newCup $ if calc > 0 then calc else 0
    where
        calc = getNum obj - num


main = do

    let cup10 = newCup 10

    print $ getNum cup10
    print $ getNum $ drink cup10 2
    print $ getNum $ foldl drink cup10 $ take 20 [1..]

    putStrLn "done."


-- EOF
