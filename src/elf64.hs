{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Binary.Get
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Bool
import Debug.Trace

import qualified Data.ByteString.Lazy as BL

main = do
    hdl <- openBinaryFile "a.out" ReadMode
    input <- BL.hGetContents hdl

    let x = runGet (runExceptT step) input

    print x


--guardE :: String -> ExceptT String m a -> Except e m ()
guardLift emsg p = do
    b <- lift p
    ExceptT $ return $ bool (Left emsg) (pure ()) b


step :: ExceptT String Get String
step = do
    guardLift "EI_MAG0" $ (== 0x7f) <$> getWord8
    guardLift "EI_MAG1-3" $ (== "ELF") <$> getByteString 3
    guardLift "EI_CLASS" $ (== 2) <$> getWord8
    guardLift "EI_DATA" $ (== 1) <$> getWord8
    guardLift "EI_VERSION" $ (== 1) <$> getWord8
    guardLift "EI_OSABI" $ (== 0) <$> getWord8
    guardLift "EI_ABIVERSION" $ (== 0) <$> getWord8

    {-
    guardLift "unuse:1" $ (== 0) <$> getWord8
    guardLift "unuse:2" $ (== 0) <$> getWord8
    guardLift "unuse:3" $ (== 0) <$> getWord8
    guardLift "unuse:4" $ (== 0) <$> getWord8
    guardLift "unuse:5" $ (== 0) <$> getWord8
    guardLift "unuse:6" $ (== 0) <$> getWord8
    guardLift "unuse:7" $ (== 0) <$> getWord8
    -}
    guardLift "unuse" $ all (== 0) <$> replicateM 7 getWord8

    guardLift "e_type" $ (== 3) <$> getWord16host
    guardLift "e_machine" $ (== 62) <$> getWord16host


    return "OK"



-- EOF
