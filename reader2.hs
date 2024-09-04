import Control.Monad.Reader

main = do
    print $ (`runReader` 1) $ do
        a <- ask                  -- 状態を確認

        b <- local (+ 1) $ do     -- ネスト
            b' <- ask             -- localによる(+ 1)が影響
            return b'

        c <- ask                  -- ネスト外では状態に変化はない

        return (a, b, c)

