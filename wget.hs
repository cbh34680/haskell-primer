{-# LANGUAGE OverloadedStrings #-}

-- import Network.BSD
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import qualified Control.Exception as E

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Extra
import Control.Arrow

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TI
import qualified Data.Text.Read as TR

import Debug.Trace



main = do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
    ai <- head <$> NS.getAddrInfo (Just hints) (Just "localhost") (Just "http")

    let sa = NS.addrAddress ai

    print ai
    print sa

    E.bracket (NS.openSocket ai) closeSocket (printHtmlContent sa)

    putStrLn "done."


closeSocket :: NS.Socket -> IO ()
closeSocket sock = do
    NS.close sock
    putStrLn "socket closed"


printHtmlContent :: NS.SockAddr -> NS.Socket -> IO ()
printHtmlContent sa sock = do

    print sock

    NS.connect sock sa

    let req = BSC.intercalate "\r\n" [
            "GET / HTTP/1.1",
            "Host: local",
            "Connection: close",
            "",
            "" ]

    BSC.putStrLn req

    NSB.sendAll sock req

    content <- runMaybeT (recvContent sock)

    when (Maybe.isJust content) $ do

        putStrLn "=== SHOW DATA === >>>"
        let Just (header, body) = content

        TI.putStrLn $ fst header
        print $ snd header
        TI.putStrLn body
        putStrLn "=== SHOW DATA === <<<"


splitByteString :: BSC.ByteString -> BSC.ByteString -> [BSC.ByteString]
splitByteString sep cs =
    let
        (l, r) = BSC.breakSubstring sep cs
    in
        if BSC.null r then [l]
            else l : splitByteString sep (BSC.drop (BSC.length sep) r)


readMaybeInt :: T.Text -> Maybe Int
readMaybeInt xs =
    case TR.decimal xs of
        Left _ -> Nothing
        Right (n, ys) -> if T.null ys then pure n else empty


type Header = (T.Text, Map.Map T.Text T.Text)

toHeader :: BSC.ByteString -> Maybe Header
toHeader cs = do
    let xs = map TE.decodeASCII $ splitByteString "\r\n" cs
    guard ((not . null) xs)

    let hdrkv = Map.fromList $ map toTuples (tail xs)

    Map.lookup "content-type" hdrkv >>= guard . T.isPrefixOf "text/"

    return (head xs, hdrkv)

    where
        toTuples cs =
            let
                (a, b) = T.break (== ':') cs
            in
                (T.toLower $ T.strip a, T.strip $ T.drop 1 b)


recvContent :: NS.Socket -> MaybeT IO (Header, T.Text)
recvContent sock = do

    (hdrtxt, rest) <- liftIO $ loopM appendChunk mempty
    hdr <- hoistMaybe $ toHeader hdrtxt

    let hdrkv = snd hdr

    clen <- hoistMaybe $ Map.lookup "content-length" hdrkv >>= readMaybeInt
    --liftIO $ print clen
    --liftIO $ print $ BSC.length rest

    let remain = clen - BSC.length rest
    --liftIO $ print remain

    if remain == 0 then return (hdr, TE.decodeUtf8 rest)
        else do
            --liftIO $ putStrLn ("-- [remain is " ++ show remain ++ "]")
            liftIO $ (hdr,) <$> (TE.decodeUtf8 . (rest <>)
                                    <$> recvSock sock (clen - BSC.length rest))

    where
        appendChunk :: BSC.ByteString -> IO (
            Either BSC.ByteString (BSC.ByteString, BSC.ByteString))

        appendChunk acc = do
            cs <- recvSock sock 16
            let acc' = acc <> cs
                (ls, rs) = BSC.breakSubstring "\r\n\r\n" acc'

            if BSC.null rs then
                return $ Left acc'
                else
                    return $ Right (ls, BSC.drop 4 rs)


recvSock sock len = do
    bs <- NSB.recv sock len

    BSC.putStrLn $ "--- [" <> BSC.pack (show $ BSC.length bs) <> " : " <> bs <> "]"

    let nbs = BSC.length bs
    guard (nbs > 0)

    if nbs == len then return bs
        else (<>) bs <$> recvSock sock (len - nbs)


-- EOF
