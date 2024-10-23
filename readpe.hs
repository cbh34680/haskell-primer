import System.IO
import System.Directory (getCurrentDirectory)
import System.FilePath (joinPath)
import Data.ByteString.Internal (w2c)
import Data.Binary.Get
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad (replicateM, guard, join)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Class ( MonadTrans(..) )

import qualified Data.ByteString.Lazy as LBS
import Debug.Trace
import Data.Maybe (isJust, fromJust, maybeToList)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (intercalate)

import Data.Time.Clock.POSIX ( POSIXTime, posixSecondsToUTCTime )
import Data.Time.LocalTime ( LocalTime, TimeZone(..), utcToLocalTime )
import Data.Binary (Word32)
import Data.Word (Word16)

jstTimeZone :: TimeZone
jstTimeZone = TimeZone { timeZoneMinutes = 9 * 60, timeZoneSummerOnly = False, timeZoneName = "JST" }

unixTimeToJST :: POSIXTime -> LocalTime
unixTimeToJST = utcToLocalTime jstTimeZone . posixSecondsToUTCTime



getC :: Get Char
getC = w2c <$> getWord8


liftI :: (Integral a, MonadTrans t, Monad m, Functor (t m), Num b) => m a -> t m b
-- liftI x = lift $ fromIntegral <$> x
liftI = lift . fmap fromIntegral
-- liftI = fmap fromIntegral . lift
-- liftI = (fromIntegral <$>) . lift
-- liftI f = fromIntegral <$> lift f

lskip :: Int -> MaybeT Get ()
lskip = lift . skip

-- http://hp.vector.co.jp/authors/VA050396/tech_06.html
-- https://learn.microsoft.com/ja-jp/windows/win32/api/winnt/ns-winnt-image_optional_header64


data ImageDosHeader = ImageDosHeader {
    e_cblp :: Integer,
    e_cp :: Integer,
    e_crlc :: Integer
} deriving Show

-- toImageDosHeader :: (Integer, Integer, Integer) -> ImageDosHeader
toImageDosHeader (a, b, c) = ImageDosHeader a b c

data Var = W16 (Get Word16) | W32 (Get Word32)

readDOSH :: MaybeT Get String
readDOSH = do
    -- IMAGE_DOS_HEADER
    guard . (== "MZ") =<< lift (replicateM 2 getC)

    e_cblp <- liftI getWord16le
    e_cp <- liftI getWord16le
    e_crlc <- liftI getWord16le

    -- let aa = sequence [getWord16le, getWord16le, getWord16le]

    lskip ((29 - 3) * 2)

    e_lfanew <- liftI getWord32le
    curr <- liftI bytesRead
    -- a <- on (-) liftI getWord32le bytesRead

    -- a <- lift $ on (-) fromIntegral getWord32le bytesRead

    lskip (e_lfanew - curr)

    return "DOS"


readNTH :: MaybeT Get String
readNTH = do
    -- IMAGE_NT_HEADERS
    guard . (== "PE\0\0") =<< lift (replicateM 4 getC)

    -- IMAGE_FILE_HEADER
    machine <- flip lookup [(0x8664, "x64"), (0x0200, "itanium"), (0x014c, "x86"), (0x0000, "unknown")] <$> lift getWord16le
    guard (isJust machine)

    numberOfSections <- liftI getWord16le
    timeDateStamp <- lift $ unixTimeToJST . fromIntegral <$> getWord32le
    pointerToSymbolTable <- liftI getWord32le
    numberOfSymbols <- liftI getWord32le
    sizeOfOptionalHeader <- liftI getWord16le
    characteristics <- liftI getWord16le

    -- IMAGE_OPTIONAL_HEADER
    optMagic <- liftI getWord16le
    let optMagic' = lookup optMagic [(0x10b, "PE"), (0x20b, "PE+"), (0x107, "ROM")]
    guard (isJust optMagic')
    guard (optMagic == 0x20b)       -- only 64bit

    linkerVersion <- lift $ (,) <$> getWord8 <*> getWord8
    sizeOfCode <- liftI getWord32le
    sizeOfInitializedData <- liftI getWord32le
    sizeOfUninitializedData <- liftI getWord32le
    addressOfEntryPoint <- liftI getWord32le
    baseOfCode <- liftI getWord32le
    baseOfData <- liftI getWord32le

    -- // NT additional fields.
    imageBase <- liftI getWord64le
    sectionAlignment <- liftI getWord32le


    let a = (
                machine, numberOfSections, timeDateStamp, pointerToSymbolTable, numberOfSymbols, sizeOfOptionalHeader, characteristics,
                linkerVersion, sizeOfCode, sizeOfInitializedData, sizeOfUninitializedData, addressOfEntryPoint, baseOfCode, baseOfData,
                imageBase
            )

    return $ "NT" ++ show a



readPE :: Get (Maybe String)
readPE = do

    -- foldl1 f <$> mapM runMaybeT [readDOSH, readNTH]
    mapM runMaybeT [readDOSH, readNTH] <&> commaSeparate

    where 
        commaSeparate = foldl1 (((return . intercalate ",") .) . on (++) maybeToList)

        -- f a b = return . join $ on (++) maybeToList a b
        -- f a = g . on (++) maybeToList a
        -- f = (g  .) . on (++) maybeToList
        -- g = return . join
        -- f = ((return . join) .) . on (++) maybeToList


    {-
    dosh <- runMaybeT readDOSH
    nth <- runMaybeT readNTH

    return $ do
                dosh' <- dosh
                nth' <- nth
                return $ dosh' ++ nth'
    -}




main :: IO ()
main = do
    xpath <- joinPath . flip (:) ["src", "a.exe"] <$> getCurrentDirectory

    withBinaryFile xpath ReadMode $ \h -> do
        dat <- LBS.hGetContents h
        -- print $ runGet readPE dat
        print $ runGet readPE dat

    print "done."

