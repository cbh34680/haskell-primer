import Data.IORef

main = do
    ref <- newIORef 0

    let closure = modifyIORef ref (+1)

    closure
    closure
    closure

    x <- readIORef ref
    print x


