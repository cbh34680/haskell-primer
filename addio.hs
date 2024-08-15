import qualified Text.Read as TR
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

readInt :: String -> Maybe Int
readInt = TR.readMaybe

{-
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    return = MaybeT . return . Just

    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
-}

--addIO :: IO (Maybe Int)
addIO :: MaybeT IO Int

addIO = do
    --ma <- fmap readInt getLine       -- ma :: Maybe Int
    --mb <- fmap readInt getLine       -- mb :: Maybe Int

    lift $ putStr "Input a >> "
    a <- MaybeT $ fmap readInt getLine  -- a :: Int

    lift $ putStr "Input b >> "
    b <- MaybeT $ fmap readInt getLine  -- b :: Int

    return $ a + b

    --return $ addMaybe a b
    --return $ (+) <$> ma <*> mb
    {-
    where
        addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
        addMaybe ma mb = do
            a <- ma
            b <- mb
            return (a + b)
    -}

addIO' = MaybeT (fmap readInt getLine) >>=
            \a -> MaybeT (fmap readInt getLine) >>=
                \b -> return (a+b)


f = do
    {-
        runMaybeT addIO により IO (Maybe Int) が取り出せるので
        その IO (Maybe Int) のアクションにより Maybe Int を取り出す --> ma
    -}
    ma <- runMaybeT addIO       -- ma :: Maybe Int
    print ma



-- EOF
