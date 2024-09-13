{-# LANGUAGE QuasiQuotes #-}

import Data.String.Here (hereLit)


main = do
    let hereText = [hereLit|
    Usage: ...

    $ ...
    $ ...

    Note: ...
    |]

    putStrLn ("{{" ++ hereText ++ "}}")
